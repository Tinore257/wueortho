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
      WeightedEdge(NodeIndex(3), NodeIndex(4), 1.0),
      WeightedEdge(NodeIndex(5), NodeIndex(6), 1.0),
      WeightedEdge(NodeIndex(5), NodeIndex(7), 1.0),
      WeightedEdge(NodeIndex(0), NodeIndex(8), 1.0),
      WeightedEdge(NodeIndex(8), NodeIndex(9), 1.0),
      WeightedEdge(NodeIndex(8), NodeIndex(10), 1.0),
      WeightedEdge(NodeIndex(9), NodeIndex(10), 1.0),
      WeightedEdge(NodeIndex(9), NodeIndex(11), 1.0),
      WeightedEdge(NodeIndex(8), NodeIndex(11), 1.0),
    )
  val graph                            = Graph.fromWeightedEdges(weightedEdges, 12).mkWeightedGraph;
  val result                           = RectilinearLayout.tarjanHopcraft(graph)
  println("Done!");
end main
object RectilinearLayout:

  case class BiNode(id: NodeIndex, var depth: Int, var lowpoint: Int, var edges: Iterator[WeightedLink])

  def tarjanHopcraft(G: WeightedGraph): (Set[NodeIndex], Seq[Set[WeightedEdge]]) =
    var result: (Set[NodeIndex], Seq[Set[WeightedEdge]]) = (Set.empty, Seq.empty)
    val cutVertices                                      = mutable.ArrayBuffer.empty[BiNode]
    val visited                                          = mutable.BitSet.empty
    var nodeArray                                        = G.vertices.zipWithIndex
      .map((v, i) => BiNode(NodeIndex(i), Integer.MAX_VALUE, Integer.MAX_VALUE, v.neighbors.iterator))

    def add(
        a: (Set[NodeIndex], Set[WeightedEdge]),
        b: (Set[NodeIndex], Set[WeightedEdge]),
    ): (Set[NodeIndex], Set[WeightedEdge]) =
      (a._1.++(b._1), a._2.++(b._2))

    def dfs(node: BiNode): (Set[NodeIndex], Set[WeightedEdge]) =
      var res: (Set[NodeIndex], Set[WeightedEdge]) = (Set.empty, Set.empty)

      var counter  = 0;
      visited.addOne(node.id.toInt)
      res = add(res, (Set(node.id), Set.empty))
      node.lowpoint = node.depth
      val outgoing = node.edges.toSeq
      while (outgoing.exists(e => !visited.contains(e.toNode.toInt))) do
        counter = counter + 1;
        // update lowpoint if visited neigbor has lower depth
        node.lowpoint = node.lowpoint min outgoing.filter(e => visited.contains((e.toNode.toInt)))
          .map(e => nodeArray(e.toNode.toInt).depth).minOption.getOrElse(node.lowpoint)
        // skip all nodes, that were already visited
        val newNeighbors = outgoing.filter(e => !visited.contains(e.toNode.toInt)).iterator
        val nextEdge     = newNeighbors.next()
        val newNode      = nodeArray(nextEdge.toNode.toInt)
        newNode.depth = node.depth + 1;
        newNode.lowpoint = node.depth + 1;
        newNode.edges = G.vertices(nextEdge.toNode.toInt).neighbors.filter(e => e.toNode != node.id).iterator;
        res = add(res, dfs(newNode))
        node.lowpoint = node.lowpoint min newNode.lowpoint
      end while
      node.lowpoint = node.lowpoint min (if outgoing.isEmpty then node.depth
                                         else outgoing.map(l => nodeArray(l.toNode.toInt).lowpoint).min)

      if node.depth > 0 then // for a non-root node
        // test, if current node v is cutVertex (has child y with lowpoint(y) >= depth(v))
        if outgoing.exists(y => nodeArray(y.toNode.toInt).lowpoint >= node.depth) then cutVertices.addOne(node)
      else if counter > 1 then
        cutVertices.addOne(node) // root-node is cut-vertex if it has more than one child in dfs tree
      res
    end dfs

    while (nodeArray.exists(node => !visited.contains(node.id.toInt))) do
      val undicoveredNodes = nodeArray.filter(n => !visited.contains(n.id.toInt))
      val component        = dfs(
        BiNode(undicoveredNodes(0).id, 0, 0, G.vertices(undicoveredNodes(0).id.toInt).neighbors.iterator),
      )
      result = (result._1.++(component._1), result._2.appended(component._2))
    end while

    result
  end tarjanHopcraft

end RectilinearLayout
