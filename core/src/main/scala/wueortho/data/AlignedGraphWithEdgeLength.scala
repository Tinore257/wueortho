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

case class AlignedWithLengthEdge(
    from: NodeIndex = NodeIndex(0),
    to: NodeIndex = NodeIndex(0),
    direction: Direction = Direction.North,
    length: Int = 0,
) derives CanEqual:
  def unweight                                        = AlignedEdge(from, to, direction)
  def fromAlignedEdge(edge: AlignedEdge, length: Int) =
    AlignedWithLengthEdge(edge.from, edge.to, edge.direction, length)
end AlignedWithLengthEdge

trait AlignedWithLengthOps:

  def getDissectedEdges(edge: AlignedEdge): Seq[AlignedWithLengthEdge]

  def getPositions(): Seq[Vec2D]

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
  end edges

  /** @param startNode
    * @param links
    * @return
    */
  def linksToEdges(startNode: NodeIndex, links: Seq[AlignedWithLengthLink]): Seq[AlignedWithLengthEdge] =
    if links.isEmpty then return Seq.empty
    val res = links.foldLeft(
      Seq(AlignedWithLengthEdge(startNode, startNode, links(0).direction, links(0).length)),
    )((acc, link) => acc.appended(AlignedWithLengthEdge(acc.last.to, link.toNode, link.direction, link.length)))
    res.drop(1)
  end linksToEdges

  /** returns the original edge given an edge of the dissected graph
    *
    * @param edge
    * @return
    */
  def getDissectedEdges(edge: AlignedEdge): Seq[AlignedWithLengthEdge] =
    var result: Seq[AlignedWithLengthLink] = Seq.empty
    var originalEdge                       = edges.find(e => e.from == edge.from && e.to == edge.to && e.direction == edge.direction)
    if originalEdge.isDefined then return Seq(originalEdge.get)
    else if vertices(edge.from.toInt).neighbors.exists(l => l.direction == edge.direction) then
      // result = result.appended(vertices(edge.from.toInt).neighbors.find(l => l.direction == edge.direction).get)
      while result.isEmpty || (result.last.toNode != edge.to && vertices(result.last.toNode.toInt).neighbors
          .exists(_.direction == edge.direction))
      do
        val lastNode = if result.isEmpty then edge.from else result.last.toNode
        result = result.appended(vertices(lastNode.toInt).neighbors.find(_.direction == edge.direction).get)
      end while
      return linksToEdges(edge.from, result)
    end if

    Seq.empty
  end getDissectedEdges

  def getPositionFromNeigbor(linkToNeigbor: AlignedWithLengthLink, position: Vec2D): Vec2D =
    linkToNeigbor.direction match
      case Direction.North => Vec2D(position.x1, position.x2 + linkToNeigbor.length)
      case Direction.East  => Vec2D(position.x1 - linkToNeigbor.length, position.x2)
      case Direction.South => Vec2D(position.x1, position.x2 - linkToNeigbor.length)
      case Direction.West  => Vec2D(position.x1 + linkToNeigbor.length, position.x2)
  end getPositionFromNeigbor

  def getPositions(): Seq[Vec2D] =
    if vertices.isEmpty then Seq.empty
    var unvisitedNodes: mutable.Set[NodeIndex] = vertices.indices.map(NodeIndex(_)).to(mutable.Set)
    var fronteer: mutable.Set[NodeIndex]       = mutable.Set()
    val position: mutable.IndexedBuffer[Vec2D] = vertices.indices.map(_ => Vec2D(0, 0)).to(mutable.IndexedBuffer)
    while !unvisitedNodes.isEmpty || !fronteer.isEmpty do
      val currentNode         = if fronteer.isEmpty then unvisitedNodes.last else fronteer.last
      unvisitedNodes.-=(currentNode)
      fronteer.-=(currentNode)
      val neigborLinksWithPos = vertices(currentNode.toInt).neighbors.filter(n => !unvisitedNodes.contains(n.toNode))
        .map(l => (l, position(l.toNode.toInt)))
      if neigborLinksWithPos.isEmpty then position(currentNode.toInt) = Vec2D(0.0, 0.0)
      else
        val newPos    = neigborLinksWithPos.map((l, pos) => getPositionFromNeigbor(l, pos))
        val isAllSame = newPos.distinct.size == 1
        if !isAllSame then sys.error("position of neigbors did not match up with edge length!")
        position(currentNode.toInt) = newPos.last
      fronteer.++=(vertices(currentNode.toInt).neighbors.map(_.toNode).filter(unvisitedNodes.contains(_)))
    end while
    position.toSeq
  end getPositions

end AwLGImpl
