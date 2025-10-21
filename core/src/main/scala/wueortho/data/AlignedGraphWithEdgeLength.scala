package wueortho.data
import scala.collection.mutable
import scala.collection.AbstractIterator
import wueortho.util.GraphConversions.all
import scala.compiletime.ops.long
import scala.compiletime.ops.double
import scala.collection.mutable.IndexedBuffer

case class AlignedWithLengthLink(toNode: NodeIndex, reverseIndex: Int, direction: Direction, length: Int)
    derives CanEqual:
  def unweight = AlignedLink(toNode, reverseIndex, direction)

case class AlignedWithLengthEdge(from: NodeIndex, to: NodeIndex, direction: Direction, length: Int) derives CanEqual:
  def unweight = AlignedEdge(from, to, direction)

trait AlignedWithLengthOps:

//def getDissectedEdges(edge: AlignedEdge): Seq[AlignedEdge]

end AlignedWithLengthOps

trait AlignedWithLengthGraph extends Graph[AlignedWithLengthLink, AlignedWithLengthEdge], AlignedWithLengthOps

/*
private def mkEdges[L, E](nodes: Seq[Vertex[L]], mk: (NodeIndex, L) => E, toBasicLink: L => BasicLink) = for
  (node, u) <- nodes.zipWithIndex
  (link, j) <- node.neighbors.zipWithIndex
  basicLink  = toBasicLink(link)
  if basicLink.toNode.toInt > u || (basicLink.toNode.toInt == u && basicLink.reverseIndex > j)
yield mk(NodeIndex(u), link) */

class AwLBuilder private (
    adj: mutable.ArrayBuffer[mutable.ArrayBuffer[(NodeIndex, Int, Direction, Int)]],
):
  private def ensureSize(i: Int) = if adj.size <= i then adj ++= Seq.fill(i - adj.size + 1)(mutable.ArrayBuffer.empty)

  def addEdge(from: NodeIndex, to: NodeIndex, direction: Direction, length: Int): AwLBuilder =

    ensureSize(from.toInt max to.toInt)
    if from == to then // beware the loops
      adj(from.toInt) += ((to, adj(from.toInt).size + 1, direction, length))
      adj(to.toInt) += ((from, adj(from.toInt).size - 1, direction, length))
    else
      adj(from.toInt) += ((to, adj(to.toInt).size, direction, length))
      adj(to.toInt) += ((from, adj(from.toInt).size - 1, direction.reverse, length))
    this
  end addEdge

  def addEdge(from: NodeIndex, to: NodeIndex): AwLBuilder = addEdge(from, to, Direction.North, 1)

  def size = adj.size

  def mkAlignedWithLengthGraph: AlignedWithLengthGraph = AwLGImpl(
    adj.map(links => Vertex(links.map((v, rl, dir, length) => AlignedWithLengthLink(v, rl, dir, length)).toIndexedSeq))
      .toIndexedSeq,
  )
end AwLBuilder

object AwLBuilder:

  def empty = AwLBuilder { mutable.ArrayBuffer.empty }

  def reserve(n: Int) = AwLBuilder(mutable.ArrayBuffer.fill(n)(mutable.ArrayBuffer.empty))

def awlBuilder() = AwLBuilder.empty

object AlignedWithLengthGraph:
  case class fromAlignedWithLengthEdges(edges: Seq[AlignedWithLengthEdge], size: Int = -1):
    def mkAlignedWithLengthGraph: AlignedWithLengthGraph =
      fromEdgesUndirected[AlignedWithLengthEdge](e => (e.from, e.to, e.direction, e.length), edges, size)
        .mkAlignedWithLengthGraph

  private def fromEdgesUndirected[E](ex: E => (NodeIndex, NodeIndex, Direction, Int), edges: Seq[E], size: Int) =
    val bld = if size < 0 then awlBuilder() else AwLBuilder.reserve(size)

    edges.map(ex).foldLeft(bld)(_.addEdge.tupled(_))
    if size >= 0 then require(bld.size == size, s"node index was out of bounds [0, $size)")
    bld

end AlignedWithLengthGraph

private case class AwLGImpl[Graph](
    nodes: IndexedSeq[Vertex[AlignedWithLengthLink]],
) extends AlignedWithLengthGraph:
  override def apply(i: NodeIndex) = nodes(i.toInt)
  override def numberOfVertices    = nodes.length
  override def numberOfEdges       = nodes.map(_.neighbors.length).sum / 2
  override def vertices            = nodes
  override lazy val edges          =
    mkEdges(nodes, (u, l) => AlignedWithLengthEdge(u, l.toNode, l.direction, l.length), _.unweight.unalign)
end AwLGImpl
