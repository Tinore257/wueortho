package wueortho.data
import scala.collection.mutable
import wueortho.data.Graph.Builder
import wueortho.data.Graph.DiBuilder

// reverseIndex: Position in der Adjazenzliste der toNode, von dem Link zur aktuellen fromNode
case class AlignedWeightedLink(toNode: NodeIndex, weight: Double, reverseIndex: Int, orientation: Direction):
  def unalign = WeightedLink(toNode, weight, reverseIndex)

case class AlignedWeightedEdge(from: NodeIndex, to: NodeIndex, weight: Double, orientation: Direction) derives CanEqual:
  def unalign = WeightedEdge(from, to, weight)

trait AlignedWeightedGraph extends Graph[AlignedWeightedLink, AlignedWeightedEdge]

private def mkEdges[L, E](nodes: Seq[Vertex[L]], mk: (NodeIndex, L) => E, toBasicLink: L => BasicLink) = for
  (node, u) <- nodes.zipWithIndex
  (link, j) <- node.neighbors.zipWithIndex
  basicLink  = toBasicLink(link)
  if basicLink.toNode.toInt > u || (basicLink.toNode.toInt == u && basicLink.reverseIndex > j)
yield mk(NodeIndex(u), link)

def builder()   = Builder.empty
def diBuilder() = DiBuilder.empty

private def fromEdgesUndirected[E](ex: E => (NodeIndex, NodeIndex, Double), edges: Seq[E], size: Int) =
  val bld = if size < 0 then builder() else Builder.reserve(size)
  edges.map(ex).foldLeft(bld)(_.addEdge.tupled(_))
  if size >= 0 then require(bld.size == size, s"node index was out of bounds [0, $size)")
  bld

private def fromEdgesDirected[E](ex: E => (NodeIndex, NodeIndex, Double), edges: Seq[E], size: Int) =
  val bld = if size < 0 then diBuilder() else DiBuilder.reserve(size)
  edges.map(ex).foldLeft(bld)(_.addEdge.tupled(_))
  if size >= 0 then require(bld.size == size, s"node index was out of bounds [0, $size)")
  bld
extension (x: Builder)
  def mkAlignedWeightedGraph: AlignedWeightedGraph = AWGImpl(
    x.adj.map(links => Vertex(links.map((v, w, rl) => AlignedWeightedLink(v, w, rl)).toIndexedSeq)).toIndexedSeq,
  )
end extension
case class fromAlignedWeightedEdges(edges: Seq[AlignedWeightedEdge], size: Int = -1):
  def mkAlignedWeightedGraph: AlignedWeightedGraph =
    fromEdgesUndirected[AlignedWeightedEdge](e => (e.from, e.to, e.weight), edges, size).mkAlignedWeightedGraph

private case class AWGImpl[Graph](
    nodes: IndexedSeq[Vertex[AlignedWeightedLink]],
) extends AlignedWeightedGraph:
  val northAligned: mutable.BitSet = mutable.BitSet.empty
  val southAligned: mutable.BitSet = mutable.BitSet.empty
  val westAligned: mutable.BitSet  = mutable.BitSet.empty
  val eastAligned: mutable.BitSet  = mutable.BitSet.empty
  override def apply(i: NodeIndex) = nodes(i.toInt)
  override def numberOfVertices    = nodes.length
  override def numberOfEdges       = nodes.map(_.neighbors.length).sum / 2
  override def vertices            = nodes
  override lazy val edges          =
    mkEdges(nodes, (u, l) => AlignedWeightedEdge(u, l.toNode, l.weight, l.orientation), _.unalign.unweighted)
end AWGImpl
