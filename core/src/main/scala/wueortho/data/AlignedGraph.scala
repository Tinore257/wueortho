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

case class SquareChain(chain: Seq[NodeIndex], chainComplement: Seq[NodeIndex], sigma: Seq[NodeIndex])

trait AlignedOps:
  def traverseAlignedFace(start: NodeIndex, direction: Direction, end: NodeIndex, cw: Boolean): Iterator[NodeIndex]
  def getLongestChainAndSquare(): SquareChain
  def applySimplifications(chain: Seq[AlignedLink]): Seq[AlignedLink]

  /** applies exhaustively edge contraction to the chain
    *
    * @param chain
    *   sequence of AlignedLinks in correct order
    * @return
    *   resulting chain
    */
  def applyEdgeContraction(chain: Seq[AlignedLink]): Seq[AlignedLink]

  /** applies exhaustively vertex delection to the chain
    *
    * @param chain
    *   sequence of AlignedLinks in correct order
    * @return
    *   resulting chain
    */
  def applyVertexDeletion(chain: Seq[AlignedLink]): Seq[AlignedLink]

  /** Calculates the longest sequence of degree 2 nodes in the graph
    * @return
    */
  def findLongestChain(): Seq[NodeIndex]
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

  def findLongestChain(): Seq[NodeIndex] =
    val allDeg2Nodes                                   = nodes.zipWithIndex.filter((n, i) => n.neighbors.size == 2).map((n, i) => NodeIndex(i));
    val allChainEdges                                  = edges.filter(e => allDeg2Nodes.contains(e.from) && allDeg2Nodes.contains(e.to))
    val allChains: mutable.Set[mutable.Set[NodeIndex]] = mutable.Set()
    for v <- allDeg2Nodes do
      val neigbors  = allChainEdges.filter(e => e.from == v || e.to == v).flatMap(e => Set(e.to, e.from)).filter(_ != v)
      val setsWithV = allChains.filter(l => l.contains(v) || neigbors.exists(n => l.contains(n)))
      if setsWithV.size == 0 then allChains.+=(mutable.Set(v))
      else if setsWithV.size == 1 then setsWithV.last.+=(v)
      else
        allChains.foreach(set => allChains.remove(set))
        allChains.+=(setsWithV.reduce(_.union(_)).union(Set(v)))
    end for
    if allChains.isEmpty then sys.error("no chain was found!")
    val longest = allChains.toSeq.sortBy(p => p.size).last.toSeq
    val chainEndNodes = longest.filter(v => vertices(v.toInt).neighbors.exists(l => !longest.contains(l.toNode)))
    var sorted        = if chainEndNodes.length > 0 then mutable.Seq(chainEndNodes(0)) else mutable.Seq.empty
    for v <- longest do
      sorted = sorted.appendedAll(
        vertices(sorted.last.toInt).neighbors.map(_.toNode).filter(n => !sorted.contains(n) && longest.contains(n)),
      )
    end for
    sorted.toSeq
  end findLongestChain

  def getLongestChainAndSquare(): SquareChain =
    val longestChain        = findLongestChain();
    val chainEndNodes       = longestChain
      .filter(v => vertices(v.toInt).neighbors.exists(l => !longestChain.contains(l.toNode)))
    val lastChainNode       = chainEndNodes.last
    val notChainNeigbors    = vertices(lastChainNode.toInt).neighbors.filter(l => !longestChain.contains(l.toNode))
    val firstEdgeDirection  = notChainNeigbors.last.direction;
    val chainComplement     = traverseAlignedFace(lastChainNode, firstEdgeDirection, chainEndNodes(0), true).toSeq.reverse
      .drop(1).reverse;
    val completeSquareNodes = longestChain.++(chainComplement)
    val completeSquareChain = completeSquareNodes.appended(longestChain(0)).sliding(2, 1).map(l =>
      val nei = vertices(l(0).toInt).neighbors
      nei.find(link => link.toNode == l(1)).get,
    ).toSeq
    val square              = applySimplifications(completeSquareChain)
    val result              = SquareChain(longestChain, chainComplement, square.map(_.toNode));
    result
  end getLongestChainAndSquare

  def applySimplifications(chain: Seq[AlignedLink]): Seq[AlignedLink] =
    var lastLength   = chain.length + 1
    var currentChain = chain
    while (currentChain.length != lastLength) do
      lastLength = currentChain.length
      currentChain = applyVertexDeletion(applyEdgeContraction(currentChain))
    end while
    currentChain
  end applySimplifications

  def applyEdgeContraction(chain: Seq[AlignedLink]): Seq[AlignedLink] =
    if chain.length < 3 then return chain
    val candidate = chain.appendedAll(chain.slice(0, 2)).sliding(3, 1).find(l =>
      l(0).direction.isHorizontal && l(1).direction.isVertical && l(2).direction.isHorizontal && l(0).direction == l(2)
        .direction ||
        l(0).direction.isVertical && l(1).direction.isHorizontal && l(2).direction.isVertical && l(0).direction == l(2)
          .direction,
    )
    // TODO: fix the links
    if !candidate.isEmpty then
      applyEdgeContraction(
        chain.map(l =>
          if l.toNode.equals(candidate.get(1).toNode) then
            AlignedLink(candidate.get(1).toNode, l.reverseIndex, candidate.get(2).direction)
          else l,
        ),
      ).filter(l => l.equals(candidate.get(1)) || l.equals(candidate.get(2)))
    else return chain
  end applyEdgeContraction

  def applyVertexDeletion(chain: Seq[AlignedLink]): Seq[AlignedLink] =
    if chain.length < 3 then return chain
    val withWrapAround = chain.appendedAll(chain.slice(0, 1))
    val candidate      = withWrapAround.sliding(2, 1)
      .find(l => l.size == 2 && l(0).direction.isHorizontal == l(1).direction.isHorizontal)
    if !candidate.isEmpty then
      val withReplacedElement = withWrapAround.sliding(2, 1).flatMap(l =>
        if l.equals(candidate.get) then Seq(AlignedLink(candidate.get(1).toNode, l(0).reverseIndex, l(0).direction))
        else if l(0).equals(candidate.get(1)) then Seq.empty
        else Seq(l(0)),
      ).toSeq
      // filter original elements
      applyVertexDeletion(withReplacedElement)
    else chain
    end if
  end applyVertexDeletion

end AGImpl
