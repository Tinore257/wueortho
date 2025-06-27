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

@main def main() =
  val weightedEdges: Seq[WeightedEdge] =
    Seq(WeightedEdge(NodeIndex(0), NodeIndex(1), 1.0), WeightedEdge(NodeIndex(1), NodeIndex(2), 1.0))
  val graph                            = Graph.fromWeightedEdges(weightedEdges, 3).mkWeightedGraph;
  val result                           = RectilinearLayout.tarjanHopcraft(graph)
object RectilinearLayout:

  case class BiNode(id: NodeIndex, var depth: Integer, var lowpoint: Integer, var edges: Iterator[WeightedLink])

  def tarjanHopcraft(G: WeightedGraph): Seq[Set[SimpleEdge]] =
    val result: Seq[Set[SimpleEdge]] = Seq.empty
    val edgeStack                    = mutable.ArrayBuffer.empty[BiNode]
    val visited                      = mutable.BitSet.empty
    var nodeArray                    = G.vertices.zipWithIndex.map((v, i) => BiNode(NodeIndex(i), 0, 0, v.neighbors.iterator))

    def dfs(node: BiNode): Set[SimpleEdge] =
      var res: Set[SimpleEdge] = Set.empty
      while (node.edges.exists(e => !visited.contains(e.toNode.toInt))) do
        // skip all nodes, that were already visited
        val newNeighbors = node.edges.filter(e => !visited.contains(e.toNode.toInt))
        if (!newNeighbors.isEmpty) then
          val nextEdge = newNeighbors.next()
          res += (SimpleEdge(node.id, nextEdge.toNode))
          val newNode  = nodeArray(nextEdge.toNode.toInt)
          newNode.depth = node.depth + 1;
          newNode.lowpoint = node.depth + 1;
          newNode.edges = G.vertices(nextEdge._1.toInt).neighbors.filter(e => e.toNode != node.id).iterator;
          res ++= (
            dfs(newNode)
          )
        end if
      end while
      node.lowpoint = node.edges.map(l => nodeArray(l.toNode.toInt).lowpoint).min
      res
    end dfs

    result
  end tarjanHopcraft

end RectilinearLayout
