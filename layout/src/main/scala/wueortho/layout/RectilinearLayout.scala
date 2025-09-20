package wueortho.layout

import wueortho.data.AlignedEdge
import wueortho.data.AlignedGraph
import wueortho.data.AlignedLink
import wueortho.data.Direction
import wueortho.data.NodeIndex

import scala.collection.mutable
import wueortho.data.TopologicalOrdering

def exampleGraph1(): AlignedGraph =
  val alignedEdges: Seq[AlignedEdge] =
    Seq(
      AlignedEdge(NodeIndex(0), NodeIndex(1), Direction.East),
      AlignedEdge(NodeIndex(1), NodeIndex(2), Direction.East),
      AlignedEdge(NodeIndex(0), NodeIndex(4), Direction.South),
      AlignedEdge(NodeIndex(1), NodeIndex(7), Direction.South),
      // AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.South),
      // AlignedEdge(NodeIndex(3), NodeIndex(4), Direction.West),
      AlignedEdge(NodeIndex(5), NodeIndex(4), Direction.North),
      AlignedEdge(NodeIndex(5), NodeIndex(6), Direction.East),
      AlignedEdge(NodeIndex(7), NodeIndex(6), Direction.South),
      AlignedEdge(NodeIndex(6), NodeIndex(8), Direction.East),
      AlignedEdge(NodeIndex(8), NodeIndex(9), Direction.East),
      AlignedEdge(NodeIndex(9), NodeIndex(10), Direction.North),
      AlignedEdge(NodeIndex(10), NodeIndex(11), Direction.North),
      AlignedEdge(NodeIndex(11), NodeIndex(2), Direction.West),
    )
  val graph                          =
    AlignedGraph.fromAlignedEdges(alignedEdges, alignedEdges.map(e => e.from.toInt max e.to.toInt).max + 1)
      .mkAlignedGraph;
  graph
end exampleGraph1

def exampleGraph2(): AlignedGraph =
  val alignedEdges: Seq[AlignedEdge] =
    Seq(
      AlignedEdge(NodeIndex(0), NodeIndex(1), Direction.East),
      AlignedEdge(NodeIndex(1), NodeIndex(2), Direction.East),
      AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.East),
      AlignedEdge(NodeIndex(1), NodeIndex(4), Direction.South),
      AlignedEdge(NodeIndex(2), NodeIndex(5), Direction.South),
      AlignedEdge(NodeIndex(4), NodeIndex(5), Direction.East),
      AlignedEdge(NodeIndex(0), NodeIndex(11), Direction.South),
      AlignedEdge(NodeIndex(3), NodeIndex(14), Direction.South),
      // AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.South),
      // AlignedEdge(NodeIndex(3), NodeIndex(4), Direction.West),
      AlignedEdge(NodeIndex(5), NodeIndex(8), Direction.South),
      AlignedEdge(NodeIndex(8), NodeIndex(7), Direction.West),
      AlignedEdge(NodeIndex(7), NodeIndex(6), Direction.West),
      AlignedEdge(NodeIndex(6), NodeIndex(9), Direction.South),
      AlignedEdge(NodeIndex(9), NodeIndex(10), Direction.East),
      AlignedEdge(NodeIndex(9), NodeIndex(12), Direction.South),
      AlignedEdge(NodeIndex(10), NodeIndex(13), Direction.South),
      AlignedEdge(NodeIndex(11), NodeIndex(12), Direction.East),
      AlignedEdge(NodeIndex(12), NodeIndex(13), Direction.East),
      AlignedEdge(NodeIndex(13), NodeIndex(14), Direction.East),
    )
  val graph                          =
    AlignedGraph.fromAlignedEdges(alignedEdges, alignedEdges.map(e => e.from.toInt max e.to.toInt).max + 1)
      .mkAlignedGraph;
  graph
end exampleGraph2

def exampleGraph3(): AlignedGraph =
  val alignedEdges: Seq[AlignedEdge] =
    Seq(
      AlignedEdge(NodeIndex(0), NodeIndex(1), Direction.East),
      AlignedEdge(NodeIndex(1), NodeIndex(2), Direction.East),
      AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.East),
      AlignedEdge(NodeIndex(1), NodeIndex(4), Direction.South),
      AlignedEdge(NodeIndex(2), NodeIndex(5), Direction.South),
      AlignedEdge(NodeIndex(4), NodeIndex(5), Direction.East),
      AlignedEdge(NodeIndex(0), NodeIndex(11), Direction.South),
      AlignedEdge(NodeIndex(3), NodeIndex(14), Direction.South),
      // AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.South),
      // AlignedEdge(NodeIndex(3), NodeIndex(4), Direction.West),
      AlignedEdge(NodeIndex(4), NodeIndex(8), Direction.South),
      AlignedEdge(NodeIndex(8), NodeIndex(7), Direction.East),
      AlignedEdge(NodeIndex(7), NodeIndex(6), Direction.East),
      AlignedEdge(NodeIndex(6), NodeIndex(10), Direction.South),
      AlignedEdge(NodeIndex(9), NodeIndex(10), Direction.East),
      AlignedEdge(NodeIndex(9), NodeIndex(12), Direction.South),
      AlignedEdge(NodeIndex(10), NodeIndex(13), Direction.South),
      AlignedEdge(NodeIndex(11), NodeIndex(12), Direction.East),
      AlignedEdge(NodeIndex(12), NodeIndex(13), Direction.East),
      AlignedEdge(NodeIndex(13), NodeIndex(14), Direction.East),
    )
  val graph                          =
    AlignedGraph.fromAlignedEdges(alignedEdges, alignedEdges.map(e => e.from.toInt max e.to.toInt).max + 1)
      .mkAlignedGraph;
  graph
end exampleGraph3

def exampleGraph4(): AlignedGraph =
  val alignedEdges: Seq[AlignedEdge] =
    Seq(
      AlignedEdge(NodeIndex(0), NodeIndex(1), Direction.South),
      AlignedEdge(NodeIndex(1), NodeIndex(2), Direction.East),
      AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.South),
      AlignedEdge(NodeIndex(3), NodeIndex(4), Direction.West),
      AlignedEdge(NodeIndex(4), NodeIndex(5), Direction.South),
      AlignedEdge(NodeIndex(5), NodeIndex(6), Direction.West),
      AlignedEdge(NodeIndex(6), NodeIndex(7), Direction.South),
      AlignedEdge(NodeIndex(7), NodeIndex(8), Direction.East),
      AlignedEdge(NodeIndex(8), NodeIndex(9), Direction.North),
      AlignedEdge(NodeIndex(9), NodeIndex(10), Direction.East),
      AlignedEdge(NodeIndex(10), NodeIndex(11), Direction.South),
      AlignedEdge(NodeIndex(11), NodeIndex(12), Direction.West),
      AlignedEdge(NodeIndex(12), NodeIndex(13), Direction.South),
      AlignedEdge(NodeIndex(13), NodeIndex(14), Direction.East),
      AlignedEdge(NodeIndex(14), NodeIndex(15), Direction.North),
      AlignedEdge(NodeIndex(15), NodeIndex(0), Direction.West),
      AlignedEdge(NodeIndex(0), NodeIndex(16), Direction.West),
      AlignedEdge(NodeIndex(0), NodeIndex(17), Direction.North),
      AlignedEdge(NodeIndex(15), NodeIndex(18), Direction.North),
      AlignedEdge(NodeIndex(15), NodeIndex(19), Direction.East),
    )
  val graph                          =
    AlignedGraph.fromAlignedEdges(alignedEdges, alignedEdges.map(e => e.from.toInt max e.to.toInt).max + 1)
      .mkAlignedGraph;
  graph
end exampleGraph4

@main def main() =
  val graph = exampleGraph4();

  val startIndex = NodeIndex(1)

  val faceIterator = graph.traverseAlignedFace(startIndex, Direction.East, NodeIndex(0), false);

  val face = faceIterator.foldLeft(Seq(startIndex))(_ :+ _)

  // val longestPath = graph.findLongestChain();
  // Java > Scala

  // val square = graph.getLongestChainAndSquare();

  /*  val b = graph.findChainInEmbedding(square);

  val ordering = TopologicalOrdering();

  val graphOrdering = ordering.createFromAlignedGraph(graph)

  val _ = graphOrdering.addBefore(NodeIndex(7), NodeIndex(15))

  val _ = graphOrdering.deleteNode(NodeIndex(7))*/

  val aligendEdge = graph.edges(1)

  val e = graph.isOuterFace(AlignedEdge(NodeIndex(0), NodeIndex(17), Direction.North))

  // val c = graph.compactFace(aligendEdge.from, aligendEdge)

  val d = graph.compactGraph()

  val result = RectilinearLayout.tarjanHopcraft(graph)
  println("Done!");
end main
object RectilinearLayout:

  case class BiNode(id: NodeIndex, var depth: Int, var lowpoint: Int, var edges: Iterator[AlignedLink])

  def tarjanHopcraft(G: AlignedGraph): (Set[NodeIndex], Seq[Set[AlignedEdge]]) =
    var result: (Set[NodeIndex]) = (Set.empty)
    val visited                  = mutable.BitSet.empty
    val nodeArray                = G.vertices.zipWithIndex
      .map((v, i) => BiNode(NodeIndex(i), Integer.MAX_VALUE, Integer.MAX_VALUE, v.neighbors.iterator))

    var allComponentEdges: Set[Set[AlignedEdge]] = Set.empty

    def add(
        a: (Set[NodeIndex], Set[AlignedEdge]),
        b: (Set[NodeIndex], Set[AlignedEdge]),
    ): (Set[NodeIndex], Set[AlignedEdge]) =
      (a._1.++(b._1), a._2.++(b._2))

    def dfs(node: BiNode): (Set[NodeIndex], Set[AlignedEdge]) =
      var res: (Set[NodeIndex], Set[AlignedEdge]) = (Set.empty, Set.empty)

      var allBranches: Set[Set[AlignedEdge]] = Set.empty

      var isArticulation: Boolean = false;
      var counter                 = 0;
      visited.addOne(node.id.toInt)
      node.lowpoint = node.depth
      val outgoing                = node.edges.toSeq

      // TODO: Testen, ob hier wirklich alle Kanten zu bereits besuchen Knoten hinzugefügt werden
      allBranches = allBranches.++(
        outgoing.filter(e => visited.contains(e.toNode.toInt))
          .map(l => Set(AlignedEdge(node.id, l.toNode, l.direction))),
      )

      while (outgoing.exists(e => !visited.contains(e.toNode.toInt))) do
        counter = counter + 1;
        // update lowpoint if visited neigbor has lower depth
        node.lowpoint = node.lowpoint min outgoing.filter(e => visited.contains((e.toNode.toInt)))
          .map(e => nodeArray(e.toNode.toInt).depth).minOption.getOrElse(node.lowpoint)
        // skip all, nodes, that, were already, visited
        val newNeighbors             = outgoing.filter(e => !visited.contains(e.toNode.toInt)).iterator
        val nextLink                 = newNeighbors.next()
        val nextEdge                 = AlignedEdge(node.id, nextLink.toNode, nextLink.direction)
        val newNode                  = nodeArray(nextLink.toNode.toInt)
        newNode.depth = node.depth + 1;
        newNode.lowpoint = node.depth + 1;
        newNode.edges = G.vertices(nextLink.toNode.toInt).neighbors.filter(e => e.toNode != node.id).iterator;
        res = add(res, dfs(newNode))
        // create edge set for current branch with rekursive edges
        val branch: Set[AlignedEdge] = res._2.+(nextEdge)

        if nodeArray(nextEdge.to.toInt).lowpoint >= node.depth then isArticulation = true
        node.lowpoint = node.lowpoint min newNode.lowpoint

        allBranches = allBranches.+(branch)

      end while
      node.lowpoint = node.lowpoint min (if outgoing.isEmpty then node.depth
                                         else outgoing.map(l => nodeArray(l.toNode.toInt).lowpoint).min)

      var returnEdges: Set[AlignedEdge] = Set.empty

      // for a non-root node: test, if current node v is cutVertex (has child y with lowpoint(y) >= depth(v))
      if node.depth > 0 && isArticulation then
        res = add(res, (Set(node.id), Set.empty))
        // each dfs branch is a biconnected component => add to allComponentEdges
        allBranches.foreach(b => allComponentEdges.+=(b))
      else if counter > 1 then
        // root-node is cut-vertex if it has more than one child in dfs tree
        res = add(res, (Set(node.id), Set.empty))
        // each dfs branch is a biconnected component => add to allComponentEdges
        allBranches.foreach(b => allComponentEdges.+=(b))
      else // no cut-vertex at all
        // merge branches to one collection
        returnEdges = allBranches.flatMap(b => b)
      end if
      (res._1, returnEdges)
    end dfs

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
