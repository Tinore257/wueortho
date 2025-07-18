package wueortho.layout

import wueortho.data.{WeightedGraph}
import scala.collection.mutable
import wueortho.data.WeightedEdge
import wueortho.data.NodeIndex
import wueortho.data.Graph
import wueortho.data.WeightedLink
import scala.collection.AbstractIterator
import wueortho.data.Direction
import wueortho.data.AlignedEdge
import wueortho.data.AlignedGraph
import wueortho.data.AlignedEdge
import wueortho.data.BasicGraph
import wueortho.data.BasicLink
import wueortho.data.AlignedLink
import wueortho.layout.RectilinearLayout.traverseAlignedFace

@main def main() =
  val alignedEdges: Seq[AlignedEdge] =
    Seq(
      AlignedEdge(NodeIndex(0), NodeIndex(1), Direction.East),
      AlignedEdge(NodeIndex(1), NodeIndex(2), Direction.East),
      AlignedEdge(NodeIndex(0), NodeIndex(4), Direction.South),
      AlignedEdge(NodeIndex(1), NodeIndex(6), Direction.South),
      AlignedEdge(NodeIndex(2), NodeIndex(3), Direction.South),
      AlignedEdge(NodeIndex(3), NodeIndex(4), Direction.West),
      AlignedEdge(NodeIndex(5), NodeIndex(4), Direction.North),
      AlignedEdge(NodeIndex(5), NodeIndex(6), Direction.East),
    )
  val graph                          =
    AlignedGraph.fromAlignedEdges(alignedEdges, 12).mkAlignedGraph;

  val startIndex = NodeIndex(0)

  val faceIterator = traverseAlignedFace(graph, startIndex, Direction.West, NodeIndex(1), true);

  val face = faceIterator.foldLeft(Seq(startIndex))(_ :+ _)

  // var face: Seq[NodeIndex] = Seq.empty;
  // while (faceIterator.hasNext) do face = face.appended(faceIterator.next())

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
        // skip all nodes, that were already visited
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

  /** Returns a iterator to traverse along a face
    *
    * @param graphWithAlignments
    * @param start
    * @param end
    * @return
    */
  def traverseAlignedFace(
      g: AlignedGraph,
      start: NodeIndex,
      direction: Direction,
      end: NodeIndex,
      cw: Boolean,
  ): Iterator[NodeIndex] =
    new AbstractIterator[NodeIndex]:
      private var current          = start
      private var currentDirection = direction
      def hasNext                  = current != end || g.vertices(current.toInt).neighbors.isEmpty
      def next(): NodeIndex        =

        def getNextDirCW(dir: Direction) = dir match
          case Direction.South => Direction.West
          case Direction.West  => Direction.North
          case Direction.North => Direction.East
          case Direction.East  => Direction.South

        def getNextDirCCW(dir: Direction) = dir match
          case Direction.South => Direction.East
          case Direction.East  => Direction.North
          case Direction.North => Direction.West
          case Direction.West  => Direction.South

        def getFirstExistingDir(node: NodeIndex, startDir: Direction, nextDir: Direction => Direction): Direction =
          val currentDir = nextDir(startDir)
          if g.vertices(node.toInt).neighbors.map(l => l.direction).contains(currentDir) then return currentDir
          else getFirstExistingDir(node, currentDir, nextDir)

        val getNextDir = cw match
          case true  => getNextDirCCW
          case false => getNextDirCW

        val nextDir  = getFirstExistingDir(current, currentDirection, getNextDir)
        val nextLink = g.vertices(current.toInt).neighbors.filter(l => l.direction == nextDir).last
        current = nextLink.toNode
        currentDirection = nextLink.direction
        current
      end next

  end traverseAlignedFace

end RectilinearLayout
