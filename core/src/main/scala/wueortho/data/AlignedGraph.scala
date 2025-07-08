package wueortho.data

case class AlignedWeightedLink(toNode: NodeIndex, weight: Double, reverseIndex: Int):
  def unalign = WeightedLink(toNode, weight, reverseIndex)

case class AlignedWeightedEdge(from: NodeIndex, to: NodeIndex, weight: Double) derives CanEqual:
  def unalign = WeightedEdge(from, to, weight)

trait AlignedWeightedGraph extends Graph[AlignedWeightedLink, AlignedWeightedEdge]

private def mkEdges[L, E](nodes: Seq[Vertex[L]], mk: (NodeIndex, L) => E, toBasicLink: L => BasicLink) = for
  (node, u) <- nodes.zipWithIndex
  (link, j) <- node.neighbors.zipWithIndex
  basicLink  = toBasicLink(link)
  if basicLink.toNode.toInt > u || (basicLink.toNode.toInt == u && basicLink.reverseIndex > j)
yield mk(NodeIndex(u), link)

private case class AWGImpl[Graph](nodes: IndexedSeq[Vertex[AlignedWeightedLink]]) extends AlignedWeightedGraph:
  override def apply(i: NodeIndex) = nodes(i.toInt)
  override def numberOfVertices    = nodes.length
  override def numberOfEdges       = nodes.map(_.neighbors.length).sum / 2
  override def vertices            = nodes
  override lazy val edges          = mkEdges(nodes, (u, l) => AlignedWeightedEdge(u, l.toNode, l.weight), _.unalign.unweighted)
end AWGImpl
