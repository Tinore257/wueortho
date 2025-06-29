package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout, WeightedGraph}
import wueortho.data.Direction
import scala.collection.mutable
import wueortho.data.WeightedEdge
import wueortho.util.mutable.DisjointSets
import wueortho.util.Monoid
import wueortho.data.NodeIndex
import wueortho.util.GraphConversions.wg2wd
import wueortho.data.Graph
import wueortho.data.SimpleEdge
import wueortho.data.VertexBoxes
import wueortho.data.WeightedDiGraph
import wueortho.util.mutable.LinearIntervalTree.Interval
import wueortho.util.mutable.LinearIntervalTree
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.ops.double
import wueortho.routing.OrthogonalVisibilityGraph.neighbor
import wueortho.data.WeightedLink
import wueortho.util.mutable.LinearIntervalTree.empty

@main def main() =
  val weightedEdges: Seq[WeightedEdge] =
    Seq(
      WeightedEdge(NodeIndex(0), NodeIndex(1), 1.0),
      WeightedEdge(NodeIndex(1), NodeIndex(2), 1.0),
      WeightedEdge(NodeIndex(2), NodeIndex(3), 1.0),
      WeightedEdge(NodeIndex(1), NodeIndex(4), 1.0),
      WeightedEdge(NodeIndex(5), NodeIndex(6), 1.0),
      WeightedEdge(NodeIndex(5), NodeIndex(7), 1.0),
    )
  val graph                            = Graph.fromWeightedEdges(weightedEdges, 8).mkWeightedGraph;
  val result                           = RectilinearLayout.tarjanHopcraft(graph)
  println("Done!");
end main
object RectilinearLayout:

  case class BiNode(id: NodeIndex, var depth: Integer, var lowpoint: Integer, var edges: Iterator[WeightedLink])

  def tarjanHopcraft(G: WeightedGraph): Seq[Set[SimpleEdge]] =
    var result: mutable.Seq[Set[SimpleEdge]] = mutable.Seq.empty
    val edgeStack                            = mutable.ArrayBuffer.empty[BiNode]
    val visited                              = mutable.BitSet.empty
    var nodeArray                            = G.vertices.zipWithIndex.map((v, i) => BiNode(NodeIndex(i), 0, 0, v.neighbors.iterator))

    def dfs(node: BiNode): Set[SimpleEdge] =
      var res: Set[SimpleEdge] = Set.empty
      visited.addOne(node.id.toInt)
      var outgoing             = node.edges.toSeq
      while (outgoing.exists(e => !visited.contains(e.toNode.toInt))) do
        // skip all nodes, that were already visited
        val newNeighbors = outgoing.filter(e => !visited.contains(e.toNode.toInt)).iterator
        val nextEdge     = newNeighbors.next()
        res += (SimpleEdge(node.id, nextEdge.toNode))
        val newNode      = nodeArray(nextEdge.toNode.toInt)
        newNode.depth = node.depth + 1;
        newNode.lowpoint = node.depth + 1;
        newNode.edges = G.vertices(nextEdge.toNode.toInt).neighbors.filter(e => e.toNode != node.id).iterator;
        res ++= (
          dfs(newNode)
        )
      end while
      node.lowpoint =
        if node.edges.isEmpty then node.depth else node.edges.map(l => nodeArray(l.toNode.toInt).lowpoint).min
      res
    end dfs

    while (nodeArray.exists(node => !visited.contains(node.id.toInt))) do
      val undicoveredNodes = nodeArray.filter(n => !visited.contains(n.id.toInt))
      result = result.appended(
        dfs(BiNode(undicoveredNodes(0).id, 0, 0, G.vertices(undicoveredNodes(0).id.toInt).neighbors.iterator)),
      )
    result.toSeq
  end tarjanHopcraft

end RectilinearLayout
