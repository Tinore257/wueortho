package wueortho.layout

import wueortho.data.{WeightedGraph}
import scala.collection.mutable
import wueortho.data.WeightedEdge
import wueortho.data.NodeIndex
import wueortho.data.Graph
import wueortho.data.WeightedLink

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
      WeightedEdge(NodeIndex(10), NodeIndex(11), 1.0),
    )
  val graph                            = Graph.fromWeightedEdges(weightedEdges, 12).mkWeightedGraph;
  val result                           = RectilinearLayout.tarjanHopcraft(graph)
  println("Done!");
end main
object RectilinearLayout:

  case class BiNode(id: NodeIndex, var depth: Int, var lowpoint: Int, var edges: Iterator[WeightedLink])

  def tarjanHopcraft(G: WeightedGraph): (Set[NodeIndex], Seq[Set[WeightedEdge]]) =
    var result: (Set[NodeIndex]) = (Set.empty)
    val visited                  = mutable.BitSet.empty
    var nodeArray                = G.vertices.zipWithIndex
      .map((v, i) => BiNode(NodeIndex(i), Integer.MAX_VALUE, Integer.MAX_VALUE, v.neighbors.iterator))

    var allComponentEdges: Set[Set[WeightedEdge]] = Set.empty

    def add(
        a: (Set[NodeIndex], Set[WeightedEdge]),
        b: (Set[NodeIndex], Set[WeightedEdge]),
    ): (Set[NodeIndex], Set[WeightedEdge]) =
      (a._1.++(b._1), a._2.++(b._2))

    def dfs(node: BiNode): (Set[NodeIndex], Set[WeightedEdge]) =
      var res: (Set[NodeIndex], Set[WeightedEdge]) = (Set.empty, Set.empty)

      var allBranches: Set[Set[WeightedEdge]] = Set.empty

      var isArticulation: Boolean = false;
      var counter                 = 0;
      visited.addOne(node.id.toInt)
      node.lowpoint = node.depth
      val outgoing                = node.edges.toSeq

      // TODO: Testen, ob hier wirklich alle Kanten zu bereits besuchen Knoten hinzugefügt werden
      allBranches = allBranches
        .++(outgoing.filter(e => visited.contains(e.toNode.toInt)).map(l => Set(WeightedEdge(node.id, l.toNode, 1.0))))

      while (outgoing.exists(e => !visited.contains(e.toNode.toInt))) do
        counter = counter + 1;
        // update lowpoint if visited neigbor has lower depth
        node.lowpoint = node.lowpoint min outgoing.filter(e => visited.contains((e.toNode.toInt)))
          .map(e => nodeArray(e.toNode.toInt).depth).minOption.getOrElse(node.lowpoint)
        // skip all nodes, that were already visited
        val newNeighbors              = outgoing.filter(e => !visited.contains(e.toNode.toInt)).iterator
        val nextLink                  = newNeighbors.next()
        val nextEdge                  = WeightedEdge(node.id, nextLink.toNode, 1.0)
        val newNode                   = nodeArray(nextLink.toNode.toInt)
        newNode.depth = node.depth + 1;
        newNode.lowpoint = node.depth + 1;
        newNode.edges = G.vertices(nextLink.toNode.toInt).neighbors.filter(e => e.toNode != node.id).iterator;
        res = add(res, dfs(newNode))
        // create edge set for current branch with rekursive edges
        val branch: Set[WeightedEdge] = res._2.+(nextEdge)

        if nodeArray(nextEdge.to.toInt).lowpoint >= node.depth then isArticulation = true
        node.lowpoint = node.lowpoint min newNode.lowpoint

        allBranches = allBranches.+(branch)

      end while
      node.lowpoint = node.lowpoint min (if outgoing.isEmpty then node.depth
                                         else outgoing.map(l => nodeArray(l.toNode.toInt).lowpoint).min)

      var returnEdges: Set[WeightedEdge] = Set.empty

      // for a non-root node: test, if current node v is cutVertex (has child y with lowpoint(y) >= depth(v))
      if node.depth > 0 && isArticulation then
        res = add(res, (Set(node.id), Set.empty))
        // each dfs branch is a biconnected component => add to allComponentEdges
        allBranches.foreach(b => allComponentEdges.+=(b))
        // res._2 = Set.empty
      else if counter > 1 then
        // res = add(res, (Set(node.id), Set.empty)) // root-node is cut-vertex if it has more than one child in dfs tree
        res = add(res, (Set(node.id), Set.empty))
        // each dfs branch is a biconnected component => add to allComponentEdges
        allBranches.foreach(b => allComponentEdges.+=(b))
      else // no cut-vertex at all
        // merge branches to one collection
        returnEdges = allBranches.flatMap(b => b)
      end if
      (res._1, returnEdges)
    end dfs // Hallo Kolla

    while (nodeArray.exists(node => !visited.contains(node.id.toInt))) do
      val undicoveredNodes = nodeArray.filter(n => !visited.contains(n.id.toInt))
      val component        = dfs(
        BiNode(undicoveredNodes(0).id, 0, 0, G.vertices(undicoveredNodes(0).id.toInt).neighbors.iterator),
      )
      result = (result.++(component._1))
    end while

    (result, allComponentEdges.toSeq)
  end tarjanHopcraft

end RectilinearLayout
