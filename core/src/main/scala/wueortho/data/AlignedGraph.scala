package wueortho.data
import scala.collection.mutable

// reverseIndex: Position in der Adjazenzliste der toNode, von dem Link zur aktuellen fromNode
case class AlignedLink(toNode: NodeIndex, reverseIndex: Int, direction: Direction):
  def unalign = BasicLink(toNode, reverseIndex)

case class AlignedEdge(from: NodeIndex, to: NodeIndex, direction: Direction) derives CanEqual:
  def unalign = SimpleEdge(from, to)

trait AlignedGraph extends Graph[AlignedLink, AlignedEdge]

private def mkEdges[L, E](nodes: Seq[Vertex[L]], mk: (NodeIndex, L) => E, toBasicLink: L => BasicLink) = for
  (node, u) <- nodes.zipWithIndex
  (link, j) <- node.neighbors.zipWithIndex
  basicLink  = toBasicLink(link)
  if basicLink.toNode.toInt > u || (basicLink.toNode.toInt == u && basicLink.reverseIndex > j)
yield mk(NodeIndex(u), link)

class ABuilder private (
    adj: mutable.ArrayBuffer[mutable.ArrayBuffer[(NodeIndex, Int, Direction)]],
):
  private def ensureSize(i: Int) = if adj.size <= i then adj ++= Seq.fill(i - adj.size + 1)(mutable.ArrayBuffer.empty)

  def addEdge(from: NodeIndex, to: NodeIndex, orientation: Direction): ABuilder =

    ensureSize(from.toInt max to.toInt)
    if from == to then // beware the loops
      adj(from.toInt) += ((to, adj(from.toInt).size + 1, orientation))
      adj(to.toInt) += ((from, adj(from.toInt).size - 1, orientation))
    else
      adj(from.toInt) += ((to, adj(to.toInt).size, orientation))
      adj(to.toInt) += ((from, adj(from.toInt).size - 1, orientation.reverse))
    this
  end addEdge

  def addEdge(from: NodeIndex, to: NodeIndex): ABuilder = addEdge(from, to, Direction.North)

  def size = adj.size

  def mkAlignedGraph: AlignedGraph = AGImpl(
    adj.map(links => Vertex(links.map((v, rl, dir) => AlignedLink(v, rl, dir)).toIndexedSeq)).toIndexedSeq,
  )
end ABuilder

object ABuilder:

  def empty = ABuilder { mutable.ArrayBuffer.empty }

  def reserve(n: Int) = ABuilder(mutable.ArrayBuffer.fill(n)(mutable.ArrayBuffer.empty))

def aBuilder() = ABuilder.empty

object AlignedGraph:
  case class fromAlignedEdges(edges: Seq[AlignedEdge], size: Int = -1):
    def mkAlignedGraph: AlignedGraph =
      fromEdgesUndirected[AlignedEdge](e => (e.from, e.to, e.direction), edges, size).mkAlignedGraph

  private def fromEdgesUndirected[E](ex: E => (NodeIndex, NodeIndex, Direction), edges: Seq[E], size: Int) =
    val bld = if size < 0 then aBuilder() else ABuilder.reserve(size)

    edges.map(ex).foldLeft(bld)(_.addEdge.tupled(_))
    if size >= 0 then require(bld.size == size, s"node index was out of bounds [0, $size)")
    bld
end AlignedGraph

private case class AGImpl[Graph](
    nodes: IndexedSeq[Vertex[AlignedLink]],
) extends AlignedGraph:
  val northAligned: mutable.BitSet = mutable.BitSet.empty
  val southAligned: mutable.BitSet = mutable.BitSet.empty
  val westAligned: mutable.BitSet  = mutable.BitSet.empty
  val eastAligned: mutable.BitSet  = mutable.BitSet.empty
  override def apply(i: NodeIndex) = nodes(i.toInt)
  override def numberOfVertices    = nodes.length
  override def numberOfEdges       = nodes.map(_.neighbors.length).sum / 2
  override def vertices            = nodes
  override lazy val edges          =
    mkEdges(nodes, (u, l) => AlignedEdge(u, l.toNode, l.direction), _.unalign)
end AGImpl
