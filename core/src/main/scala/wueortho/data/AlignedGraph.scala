package wueortho.data
import scala.collection.mutable
import scala.collection.AbstractIterator
import wueortho.util.GraphConversions.all
import scala.compiletime.ops.long

// reverseIndex: Position in der Adjazenzliste der toNode, von dem Link zur aktuellen fromNode
case class AlignedLink(toNode: NodeIndex, reverseIndex: Int, direction: Direction):
  def unalign = BasicLink(toNode, reverseIndex)

case class AlignedEdge(from: NodeIndex, to: NodeIndex, direction: Direction) derives CanEqual:
  def unalign = SimpleEdge(from, to)

case class SquarePath(path: Seq[NodeIndex], pathComplement: Seq[NodeIndex], sigma: Seq[NodeIndex])

trait AlignedOps:
  def traverseAlignedFace(start: NodeIndex, direction: Direction, end: NodeIndex, cw: Boolean): Iterator[NodeIndex]
  def getLongestPathAndSquare(): SquarePath
  def applySimplifications(path: Seq[AlignedLink]): Seq[AlignedLink]

  /** applies exhaustively edge contraction to the path
    *
    * @param path
    *   sequence of AlignedLinks in correct order
    * @return
    *   resulting path
    */
  def applyEdgeContraction(path: Seq[AlignedLink]): Seq[AlignedLink]

  /** applies exhaustively vertex delection to the path
    *
    * @param path
    *   sequence of AlignedLinks in correct order
    * @return
    *   resulting path
    */
  def applyVertexDeletion(path: Seq[AlignedLink]): Seq[AlignedLink]

  /** Calculates the longest sequence of degree 2 nodes in the graph
    * @return
    */
  def findLongestPath(): Seq[NodeIndex]
end AlignedOps

trait AlignedGraph extends Graph[AlignedLink, AlignedEdge], AlignedOps

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

  def addEdge(from: NodeIndex, to: NodeIndex, direction: Direction): ABuilder =

    ensureSize(from.toInt max to.toInt)
    if from == to then // beware the loops
      adj(from.toInt) += ((to, adj(from.toInt).size + 1, direction))
      adj(to.toInt) += ((from, adj(from.toInt).size - 1, direction))
    else
      adj(from.toInt) += ((to, adj(to.toInt).size, direction))
      adj(to.toInt) += ((from, adj(from.toInt).size - 1, direction.reverse))
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

  /** Returns a iterator to traverse along a face
    *
    * @param start
    *   NodeIndex to start with
    * @param direction
    *   direction to start traversal
    * @param end
    *   NodeIndex to stop traversal
    * @param cw
    *   direction of traversal
    * @return
    */
  def traverseAlignedFace(
      start: NodeIndex,
      direction: Direction,
      end: NodeIndex,
      cw: Boolean,
  ): Iterator[NodeIndex] =
    new AbstractIterator[NodeIndex]:
      private var current          = start
      private var currentDirection = direction
      def hasNext                  = current != end || nodes(current.toInt).neighbors.isEmpty
      def next(): NodeIndex        =

        def getFirstExistingDir(node: NodeIndex, startDir: Direction, nextDir: Direction => Direction): Direction =
          val currentDir = nextDir(startDir)
          if nodes(node.toInt).neighbors.map(l => l.direction).contains(currentDir) then return currentDir
          else getFirstExistingDir(node, currentDir, nextDir)

        val getNextDir = cw match
          case true  => Direction.turnCW
          case false => Direction.turnCCW

        val nextDir  = getFirstExistingDir(current, currentDirection.reverse, getNextDir)
        val nextLink = nodes(current.toInt).neighbors.filter(l => l.direction == nextDir).last
        current = nextLink.toNode
        currentDirection = nextLink.direction
        current
      end next

  end traverseAlignedFace

  def findLongestPath(): Seq[NodeIndex] =
    val allDeg2Nodes                                  = nodes.zipWithIndex.filter((n, i) => n.neighbors.size == 2).map((n, i) => NodeIndex(i));
    val allPathEdges                                  = edges.filter(e => allDeg2Nodes.contains(e.from) && allDeg2Nodes.contains(e.to))
    val allPaths: mutable.Set[mutable.Set[NodeIndex]] = mutable.Set()
    for v <- allDeg2Nodes do
      val neigbors  = allPathEdges.filter(e => e.from == v || e.to == v).flatMap(e => Set(e.to, e.from)).filter(_ != v)
      val setsWithV = allPaths.filter(l => l.contains(v) || neigbors.exists(n => l.contains(n)))
      if setsWithV.size == 0 then allPaths.+=(mutable.Set(v))
      else if setsWithV.size == 1 then setsWithV.last.+=(v)
      else
        allPaths.foreach(set => allPaths.remove(set))
        allPaths.+=(setsWithV.reduce(_.union(_)).union(Set(v)))
    end for
    if allPaths.isEmpty then sys.error("no path/chain was found!")
    val longest = allPaths.toSeq.sortBy(p => p.size).last.toSeq
    val pathEndNodes = longest.filter(v => vertices(v.toInt).neighbors.exists(l => !longest.contains(l.toNode)))
    var sorted       = if pathEndNodes.length > 0 then mutable.Seq(pathEndNodes(0)) else mutable.Seq.empty
    for v <- longest do
      sorted = sorted.appended(
        vertices(sorted.last.toInt).neighbors.map(_.toNode).filter(v => !sorted.contains(v)).last,
      )
    end for
    sorted.toSeq
  end findLongestPath

  def getLongestPathAndSquare(): SquarePath =
    val longestPath         = findLongestPath();
    val pathEndNodes        = longestPath.filter(v => vertices(v.toInt).neighbors.exists(l => !longestPath.contains(l.toNode)))
    val lastPathNode        = pathEndNodes.last
    val notPathNeigbors     = vertices(lastPathNode.toInt).neighbors.filter(l => !longestPath.contains(l.toNode))
    val firstEdgeDirection  = notPathNeigbors.last.direction;
    val pathComplement      = traverseAlignedFace(lastPathNode, firstEdgeDirection, pathEndNodes(0), true).toSeq.reverse
      .drop(1).reverse;
    val completeSquareNodes = longestPath.++(pathComplement)
    val completeSquarePath  = completeSquareNodes.sliding(2, 1).map(l =>
      val nei = vertices(l(0).toInt).neighbors
      nei.find(link => link.toNode == l(1)).get,
    ).toSeq
    val square              = applySimplifications(completeSquarePath)
    val result              = SquarePath(longestPath, pathComplement, Seq.empty);
    result
  end getLongestPathAndSquare

  def applySimplifications(path: Seq[AlignedLink]): Seq[AlignedLink] =
    var lastLength  = path.length + 1
    var currentPath = path
    while (currentPath.length != lastLength) do
      lastLength = currentPath.length
      currentPath = applyVertexDeletion(applyEdgeContraction(currentPath))
    end while
    currentPath
  end applySimplifications

  def applyEdgeContraction(path: Seq[AlignedLink]): Seq[AlignedLink] =
    // TODO: deal with the wrap to the beginning
    val candidate = path.sliding(3, 1).find(l =>
      l(0).direction.isHorizontal && l(1).direction.isVertical && l(2).direction.isHorizontal ||
        l(0).direction.isVertical && l(1).direction.isHorizontal && l(2).direction.isVertical,
    )
    // TODO: fix the links
    if !candidate.isEmpty then
      applyEdgeContraction(
        path.map(l =>
          if l.toNode.equals(candidate.get(1).toNode) then
            AlignedLink(candidate.get(1).toNode, l.reverseIndex, candidate.get(2).direction)
          else l,
        ),
      ).filter(l => l.equals(candidate.get(1)) || l.equals(candidate.get(2)))
    else return path
  end applyEdgeContraction

  def applyVertexDeletion(path: Seq[AlignedLink]): Seq[AlignedLink] =
    // TODO: deal with the wrap to the beginning
    val candidate = path.sliding(2, 1).find(l => l(0).direction.isHorizontal == l(1).direction.isHorizontal)
    if !candidate.isEmpty then
      applyVertexDeletion(
        path.map(l =>
          // add skip-element
          if l.toNode.equals(candidate.get(0).toNode) then
            AlignedLink(candidate.get(1).toNode, l.reverseIndex, l.direction)
          else l,
          // filter original elements
        ).filter(l => !candidate.contains(l)),
      )
    else path
    end if
  end applyVertexDeletion

end AGImpl
