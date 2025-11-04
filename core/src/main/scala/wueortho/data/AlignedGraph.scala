package wueortho.data
import scala.collection.mutable
import scala.collection.AbstractIterator
import wueortho.util.GraphConversions.all
import scala.compiletime.ops.long
import scala.compiletime.ops.double
import scala.collection.mutable.IndexedBuffer
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.graph.DefaultEdge
import scala.collection.immutable.HashMap
import org.jgrapht.alg.flow.mincost.CapacityScalingMinimumCostFlow
import org.jgrapht.alg.flow.mincost.MinimumCostFlowProblem.MinimumCostFlowProblemImpl
import org.jgrapht.alg.interfaces.MinimumCostFlowAlgorithm.MinimumCostFlow
import org.jgrapht.alg.flow.mincost.MinimumCostFlowProblem
import java.util.Map.Entry
import scala.jdk.CollectionConverters.*

// reverseIndex: Position in der Adjazenzliste der toNode, von dem Link zur aktuellen fromNode
case class AlignedLink(toNode: NodeIndex, reverseIndex: Int, direction: Direction) derives CanEqual:
  def unalign = BasicLink(toNode, reverseIndex)

case class AlignedEdge(from: NodeIndex, to: NodeIndex, direction: Direction) derives CanEqual:
  def unalign = SimpleEdge(from, to)

case class SquareChain(chain: Seq[NodeIndex], chainComplement: Seq[NodeIndex], sigma: Seq[NodeIndex])

trait AlignedOps:
  def traverseAlignedFace(
      start: NodeIndex,
      direction: Direction,
      end: NodeIndex,
      cw: Boolean,
      cyclic: Boolean = false,
  ): Iterator[NodeIndex]

  def traverseEdgesAlignedFace(
      startEdge: AlignedEdge,
      cw: Boolean,
      cyclic: Boolean = false,
  ): Iterator[AlignedEdge]

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

  /** gets a path that starts and ends in the same direction as p that simplifies to sigma
    * @param pathAndSquare
    *   SquareChain of the path
    * @return
    */
  def findChainInEmbedding(pathAndSquare: SquareChain): Seq[AlignedLink]

  // def applyOperationsAndTransform(): Seq[AlignedLink]
  def isOuterFace(startEdge: AlignedEdge): Boolean

  def rectangularDissectFace(
      startNode: NodeIndex,
      startEdge: AlignedEdge,
  ): (newEdges: Seq[AlignedEdge], replacedEdges: Seq[AlignedEdge])

  def rectangularDissection(): AlignedGraph

  def getBottomRightCorner(seq: Seq[AlignedEdge]): NodeIndex

  def createFlowNetwork(
      dir: Direction = Direction.North,
  ): (DefaultDirectedGraph[Int, DefaultEdge], Map[DefaultEdge, Seq[AlignedEdge]])

  def solveFlowNetwork(network: org.jgrapht.Graph[Int, DefaultEdge]): Seq[(DefaultEdge, Double)]

  def determineEdgeLength(): Seq[AlignedWithLengthEdge]

  def positionsFromEdgeLength(): VertexLayout

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
      cyclic: Boolean = false,
  ): Iterator[NodeIndex] =
    new AbstractIterator[NodeIndex]:
      private val linkIterator: Iterator[AlignedLink] =
        traverseAlignedLinksFace(nodes(start.toInt).neighbors.filter(n => n.direction == direction)(0), cw, cyclic)
      def hasNext                                     = linkIterator.hasNext
      def next(): NodeIndex                           =
        val currentLink = linkIterator.next()
        currentLink.toNode
      end next

    end new

  end traverseAlignedFace

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
  def traverseEdgesAlignedFace(
      startEdge: AlignedEdge,
      cw: Boolean = false,
      cyclic: Boolean = false,
  ): Iterator[AlignedEdge] =
    new AbstractIterator[AlignedEdge]:
      private val linkIterator: Iterator[AlignedLink] =
        traverseAlignedLinksFace(
          vertices(startEdge.from.toInt).neighbors
            .filter(link => link.direction == startEdge.direction && link.toNode == startEdge.to)(0),
          cw,
          cyclic,
        )
      private var lastNode: NodeIndex                 = startEdge.to
      def hasNext                                     = linkIterator.hasNext
      def next(): AlignedEdge                         =
        val currentLink = linkIterator.next()
        val currentEdge = AlignedEdge(lastNode, currentLink.toNode, currentLink.direction)
        lastNode = currentLink.toNode
        currentEdge
      end next

    end new

  end traverseEdgesAlignedFace

  /** Returns a iterator to traverse along a face
    *
    * @param start
    *   NodeIndex to start with
    * @param end
    *   NodeIndex to stop traversal
    * @param cw
    *   direction of traversal
    * @return
    */
  def traverseAlignedLinksFace(
      start: AlignedLink,
      cw: Boolean,
      cyclic: Boolean = false,
  ): Iterator[AlignedLink] =
    new AbstractIterator[AlignedLink]:
      private var current                          = start.toNode
      private var currentDirection                 = start.direction
      private var startLink: Option[AlignedLink]   = None;
      private var isFirstLink                      = true;
      private var currentLink: Option[AlignedLink] = None;
      private var counter                          = 0;

      def isStartLink(link: AlignedLink): Boolean =
        startLink.isDefined && link.equals(startLink.get)
      def hasNext                                 = (cyclic || (isFirstLink || !isFirstLink && currentLink.isDefined && startLink
        .isDefined && !(isStartLink(currentLink.get)))) && !nodes(current.toInt).neighbors.isEmpty
      //  .isDefined && !(currentLink.get.equals(startLink.get)))) && !nodes(current.toInt).neighbors.isEmpty
      def next(): AlignedLink                     =
        startLink match
          case Some(link) => isFirstLink = false
          case None       => ()

        def getFirstExistingDir(node: NodeIndex, startDir: Direction, nextDir: Direction => Direction): Direction =
          val currentDir = nextDir(startDir)
          if nodes(node.toInt).neighbors.map(l => l.direction).contains(currentDir) then return currentDir
          else getFirstExistingDir(node, currentDir, nextDir)

        val getNextDir = cw match
          case true  => Direction.turnCW
          case false => Direction.turnCCW

        val nextDir  = getFirstExistingDir(current, currentDirection.reverse, getNextDir)
        val nextLink = nodes(current.toInt).neighbors.filter(l => l.direction == nextDir).last
        startLink match
          case Some(value) => ()
          case None        => startLink = Some(nextLink)

        counter = counter + 1;
        if counter > 2 * (vertices.length * 3 - 6) then sys.error("The provided graph can not be planar!")
        currentLink = Some(nextLink)
        current = nextLink.toNode
        currentDirection = nextLink.direction
        nextLink
      end next
    end new

  end traverseAlignedLinksFace

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
    val chainComplement     = Seq(notChainNeigbors(0).toNode).++(
      traverseAlignedFace(
        notChainNeigbors(0).toNode,
        firstEdgeDirection,
        chainEndNodes(0),
        true,
      ).toSeq.reverse.drop(1).reverse,
    );
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
      val newLinks = chain.filter(!_.toNode.equals(candidate.get(1).toNode)).map(l =>
        if l.toNode.equals(candidate.get(0).toNode) then
          AlignedLink(candidate.get(1).toNode, l.reverseIndex, candidate.get(2).direction)
        else l,
      )
      return applyEdgeContraction(
        newLinks,
      ) // .filter(l => l.equals(candidate.get(1)) || l.equals(candidate.get(2)))
    else return chain
    end if
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

  def findChainInEmbedding(pathAndSquare: SquareChain): Seq[AlignedLink] =
    var res: mutable.Seq[AlignedLink] = mutable.Seq.empty
    if pathAndSquare.chain.length < 1 then return Seq.empty
    val startNeigbors                 = vertices(pathAndSquare.chainComplement.last.toInt).neighbors
    val startLink                     = startNeigbors.filter(l => l.toNode == pathAndSquare.chain(0))
    val endNeigbors                   = vertices(pathAndSquare.chain.last.toInt).neighbors
    val endLink                       = endNeigbors.filter(l => l.toNode == pathAndSquare.chainComplement(0))
    val endDir                        = endLink.map(_.direction).reverse.last
    val complementLinks               = pathAndSquare.chainComplement.reverse.sliding(2, 1)
      .map(l => vertices(l(0).toInt).neighbors.filter(_.toNode == l(1)).last).toSeq
    res = res.appended(startLink(0))
    while complementLinks(0).direction != res.last.direction.turnCCW do
      res = res.appended(AlignedLink(NodeIndex(vertices.length + res.length - 1), 1, res.last.direction.turnCCW))
    end while
    res = res.appendedAll(complementLinks)
    while endDir != res.last.direction.turnCCW do
      res = res.appended(AlignedLink(NodeIndex(vertices.length + res.length - 1), 1, res.last.direction.turnCCW))
    end while
    res = res.appended(endLink(0))
    return res.toSeq
  end findChainInEmbedding

  def splitEdge(
      edge: AlignedEdge,
      currentVertices: IndexedSeq[Vertex[AlignedLink]] = vertices,
  ): (newNode: NodeIndex, newEdges: Seq[AlignedEdge], allNewEdges: Seq[AlignedEdge]) =
    val index         = NodeIndex(currentVertices.size)
    val edgesWithoutE = edges.filter(e => e != edge && !(e.to == edge.from && e.from == edge.to))
    val newEdges      = Seq(AlignedEdge(edge.from, index, edge.direction), AlignedEdge(index, edge.to, edge.direction))
    val allEdges      = edgesWithoutE.++(newEdges)
    (index, newEdges, allEdges)
  end splitEdge

  def getReverseEdge(edge: AlignedEdge): AlignedEdge =
    AlignedEdge(edge.to, edge.from, edge.direction.reverse)

  def nodeAndReversNode(edge: AlignedEdge): Seq[AlignedEdge] =
    Seq(edge, getReverseEdge(edge))

  def updateEdgesAfterSplit(
      edges: mutable.IndexedBuffer[AlignedEdge],
      changes: (newEdges: Seq[AlignedEdge], replacedEdges: Seq[AlignedEdge]),
  ): mutable.IndexedBuffer[AlignedEdge] =
    val allReplaced = changes.replacedEdges.flatMap(e => Seq(e, getReverseEdge(e)))
    val allNew      = changes.newEdges.flatMap(e => Seq(e, getReverseEdge(e)))
    edges.map(e =>
      if allReplaced.contains(e) then
        allNew.find(x => x.direction == e.direction && (x.from == e.from || x.to == e.to)) match
          case Some(splitEdge) => splitEdge
          case None            => sys.error("was replaced but no replacement found!")
      else e,
    )
  end updateEdgesAfterSplit

  def isOuterFace(face: Seq[AlignedEdge]): Boolean =
    def linksCornersToNumbers(a: AlignedEdge, b: AlignedEdge): Seq[Int] =
      if a.direction.turnCCW == b.direction then Seq(1)
      else if a.direction.reverse == b.direction then Seq(1, 1)
      else if a.direction == b.direction then Seq()
      else Seq(-1)
    val angleSeq                                                        = face.sliding(2).flatMap(l => linksCornersToNumbers(l(0), l(1))).toSeq
    val sum                                                             = angleSeq.reduce(_ + _)
    sum > 0
  end isOuterFace

  def isOuterFace(startEdge: AlignedEdge): Boolean =
    isOuterFace(this.traverseEdgesAlignedFace(startEdge, false).toSeq)
  end isOuterFace

  def rectangularDissectFace(
      startNode: NodeIndex,
      startEdge: AlignedEdge,
  ): (newEdges: Seq[AlignedEdge], replacedEdges: Seq[AlignedEdge]) =
    def checkForSequenceAtEnd(seq: IndexedSeq[AlignedEdge]): Boolean =
      def linksCornersToNumbers(a: AlignedEdge, b: AlignedEdge): Seq[Int] =
        if a.direction.turnCCW == b.direction then Seq(1)
        else if a.direction.reverse == b.direction then Seq(1, 1)
        else Seq(0)
      if seq.size < 4 then return false
      val s                                                               = seq.takeRight(4).sliding(2).flatMap(l => linksCornersToNumbers(l(0), l(1))).toSeq
      s.takeRight(3).equals(Seq(0, 0, 1)) || s.equals(Seq(0, 0, 1, 1))

    val res: mutable.Buffer[AlignedEdge]          = mutable.Buffer.empty
    val removedEdges: mutable.Buffer[AlignedEdge] = mutable.Buffer.empty

    if vertices.size < 5 then return (edges, Seq.empty)
    val recentEdges: mutable.IndexedBuffer[AlignedEdge] = mutable.IndexedBuffer.empty
    var lastDirectionLink                               = startEdge
    val faceIterator                                    = this.traverseEdgesAlignedFace(startEdge, false, true)
    val debug                                           = this.traverseEdgesAlignedFace(startEdge, false, true).take(40).toSeq
    var stop                                            = false
    var startNodeCounter                                = 0
    while !stop do
      var nextEdge = faceIterator.next()
      if nextEdge.to == startNode then startNodeCounter = startNodeCounter + 1
      while nextEdge.direction.equals(lastDirectionLink.direction) && startNodeCounter < 4 do
        nextEdge = faceIterator.next()
        if nextEdge.to == startNode then startNodeCounter = startNodeCounter + 1
      end while
      lastDirectionLink = nextEdge
      recentEdges.addOne(nextEdge)
      if startNodeCounter > 3 then stop = true
      if checkForSequenceAtEnd(recentEdges.toIndexedSeq) then
        // replace sequence
        val sequence                                   = recentEdges.takeRight(4)
        // TODO: Instead of inserting to sequence(2) ====> SplitEdge funktion, um neuen Knoten einzufügen!!!!!
        val (newIndex, newEdgesFromSplit, allNewEdges) = splitEdge(sequence(0))
        val newEdge                                    = AlignedEdge(sequence(3).from, newIndex, sequence(1).direction.reverse)
        val recursiveGraph                             = AlignedGraph.fromAlignedEdges(allNewEdges.appended(newEdge)).mkAlignedGraph
        val rec                                        = recursiveGraph.rectangularDissectFace(newEdge.from, getReverseEdge(newEdge))
        res.addAll((newEdgesFromSplit.appended(newEdge)).filter(e => !rec.replacedEdges.contains(e)))
        res.addAll(rec.newEdges)
        // res.--=(nodeAndReversNode(sequence(0)))
        if !(rec.newEdges.contains(sequence(0))) then removedEdges.addOne(sequence(0))
        res.--=(rec.replacedEdges.flatMap(r => nodeAndReversNode(r)))
        removedEdges.addAll(rec.replacedEdges.filter(e => !(newEdgesFromSplit.appended(newEdge)).contains(e)))
        stop = true
      end if
    end while
    (res.toSeq, removedEdges.toSeq)
  end rectangularDissectFace

  def getBottomRightCorner(seq: Seq[AlignedEdge]): NodeIndex =
    // val allConseqPairOnOuterFace = seq.++(seq.take(1)).sliding(2).filter(l => isOuterFace(l(0)))
    val allConseqPairOnOuterFace = seq.sliding(2).filter(l => isOuterFace(l(0)))
    val allBRCornersCandiates    = allConseqPairOnOuterFace.filter(l =>
      (l(0).direction == Direction.East && l(1).direction == Direction.North ||
        l(0).direction == Direction.East && l(1).direction == Direction.West ||
        l(0).direction == Direction.South && l(1).direction == Direction.North),
    ).toSeq
    val allBRCorners             = allBRCornersCandiates.map(l => l(1).from)
    /*val allBRCorners             = allBRCornersCandiates.map(l => l(1)).filter(x =>
      seq.find(e =>
        (e.direction == Direction.South && e.from == x.to) || (
          e.direction == Direction.North && e.to == x.to
        ),
      ).isEmpty,
    ).map(_.to) */
    if allBRCorners.isEmpty then sys.error("Outer face does not contain any bottom right corner!")
    allBRCorners(0)
  end getBottomRightCorner

  def getAllFaces(): mutable.IndexedBuffer[Seq[AlignedEdge]] =
    def ensureSmallestEdgeFirst(face: Seq[AlignedEdge]): Seq[AlignedEdge] =
      if face.isEmpty then return face
      val smallestElementIndex = face.zipWithIndex.minBy((e, _) => e.from.toInt)._2
      val sortedFace           = face.slice(smallestElementIndex, face.length).++(face.slice(0, smallestElementIndex))
      sortedFace
    end ensureSmallestEdgeFirst

    var unhandledEdges                                   = this.edges.to(IndexedBuffer)
    var faceEdgeCandidate: mutable.Set[Seq[AlignedEdge]] = mutable.Set().empty
    while unhandledEdges.size > 0 do
      val edge           = unhandledEdges.toSeq(0)
      val edgesFromFace1 = Seq(edge).++(traverseEdgesAlignedFace(edge, false).toSeq).dropRight(2)
      faceEdgeCandidate.+=(ensureSmallestEdgeFirst(edgesFromFace1))
      val edgesFromFace2 = Seq(getReverseEdge(edge)).++(traverseEdgesAlignedFace(getReverseEdge(edge), false).toSeq)
        .dropRight(2)
      faceEdgeCandidate += (ensureSmallestEdgeFirst(edgesFromFace2))
      unhandledEdges.--=(edgesFromFace1.toSet.++(edgesFromFace2.toSet))
    end while
    faceEdgeCandidate.to(IndexedBuffer)
  end getAllFaces

  def getOneEdgePerFace(): mutable.IndexedBuffer[AlignedEdge] =
    getAllFaces().flatMap(f => if (f.size > 0) then f.sortBy(l => l.from.toInt min l.to.toInt).take(1) else Seq.empty)
  end getOneEdgePerFace

  def rectangularDissection(): AlignedGraph =
    def addBoundingBox(edges: Seq[AlignedEdge]): Seq[AlignedEdge] =
      val i        = edges.flatMap(e => Seq(e.from, e.to)).map(_.toInt).max + 1
      val newEdges = Seq(
        AlignedEdge(NodeIndex(i), NodeIndex(i + 1), Direction.West),
        AlignedEdge(NodeIndex(i + 1), NodeIndex(i + 2), Direction.West),
        AlignedEdge(NodeIndex(i + 2), NodeIndex(i + 3), Direction.North),
        AlignedEdge(NodeIndex(i + 3), NodeIndex(i + 4), Direction.East),
        AlignedEdge(NodeIndex(i + 4), NodeIndex(i), Direction.South),
      )
      newEdges
    end addBoundingBox
    var faceEdgeCandidate                                         = getOneEdgePerFace()
    var allEdges                                                  = this.edges.toSet
    val bb                                                        = addBoundingBox(allEdges.toSeq)
    allEdges.++=(bb)
    var currentGraph                                              = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
    var i                                                         = 0
    while i < faceEdgeCandidate.size do

      var e = faceEdgeCandidate(i)
      if currentGraph.isOuterFace(e) then
        val edgesAfterCompaction = currentGraph.rectangularDissectFace(e.to, e)
        // update edges that were split
        faceEdgeCandidate = updateEdgesAfterSplit(faceEdgeCandidate, edgesAfterCompaction)
        val allReplaced          = edgesAfterCompaction.replacedEdges.flatMap(e => nodeAndReversNode(e))
        allEdges = allEdges.++(edgesAfterCompaction.newEdges).toSet.--(allReplaced)
        val compactedGraph       = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
        // after compaction e might no longer lay on the outer face
        val x                    = edgesAfterCompaction.newEdges.flatMap(e => Seq(e, getReverseEdge(e)))
          .map(e => (e, compactedGraph.isOuterFace(e)))
        e =
          if edgesAfterCompaction.newEdges.isEmpty then e
          else edgesAfterCompaction.newEdges.find(e => compactedGraph.isOuterFace(e)).get
        val debug                = compactedGraph.traverseEdgesAlignedFace(e, false).take(40).toSeq
        val outerFaceIter        = compactedGraph.traverseEdgesAlignedFace(e, false).toSeq
        val nodeToOuterFace      = compactedGraph.getBottomRightCorner(
          outerFaceIter,
        )
        val edgesToBB            = Seq(
          AlignedEdge(nodeToOuterFace, bb(0).to, Direction.South),
          // AlignedEdge(bb(0).to, nodeToOuterFace, Direction.North), // nur eine Kante hinzufügen, da auch nur eine neue Facette entsteht
        )
        allEdges.++=(edgesToBB)
        val graphWithConnectedBB = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
        faceEdgeCandidate = faceEdgeCandidate.++(edgesToBB)
        currentGraph = graphWithConnectedBB
      else
        val edgesAfterCompaction = currentGraph.rectangularDissectFace(e.to, e)
        // update edges that were split
        faceEdgeCandidate = updateEdgesAfterSplit(faceEdgeCandidate, edgesAfterCompaction)
        val allReplaced          = edgesAfterCompaction.replacedEdges.flatMap(e => nodeAndReversNode(e))
        allEdges = allEdges.++(edgesAfterCompaction.newEdges).toSet.--(allReplaced)
        val compactedGraph       = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
        currentGraph = compactedGraph
      end if
      i = i + 1
    end while
    AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
  end rectangularDissection

  def getMapEdgeToAdjacentFace(facesReps: IndexedBuffer[AlignedEdge]): Map[AlignedEdge, IndexedBuffer[Int]] =
    /*def isSameOrReverseEdge(e1: AlignedEdge, e2: AlignedEdge): Boolean =
      def isSameEdge(e1: AlignedEdge, e2: AlignedEdge): Boolean =
        e1.from == e2.from && e1.to == e2.to
      isSameEdge(e1, e2) || isSameEdge(e1, getReverseEdge(e2))*/
    def edgeSortedFromTo(e: AlignedEdge): AlignedEdge =
      if (e.from.toInt < e.to.toInt) then e else getReverseEdge(e)
    val allEdges                                      = facesReps.zipWithIndex.flatMap((rep, i) =>
      this.traverseEdgesAlignedFace(rep, false).toSeq.dropRight(1).map(edgeSortedFromTo(_)).map(l => (i, l)),
    )
    val groupedEdges                                  = allEdges.groupBy((_, edge) => edge)
    val edgeToFaceMap                                 = groupedEdges.map((k, v) => (k, v.map((face, _) => face)))
    val bothEdgesToFaceMap                            = edgeToFaceMap.flatMap((k, v) => Seq((k, v), (getReverseEdge(k), v)))
    bothEdgesToFaceMap
  end getMapEdgeToAdjacentFace

  def getCornerDirections(e1: AlignedEdge, e2: AlignedEdge): Option[(Direction, Direction)] =
    if isCorner(e1, e2) then Some(e1.direction, e2.direction) else Option.empty
  end getCornerDirections

  // eventuell zusätzlich die Facette mitgeben, damit sichergestellt werden kann, das entlang der richtgen Facette traversiert wird
  // returns all edges of a face, that close the face into the given direction
  def getAllEdgesOfFaceInDirection(
      face: Int,
      allFacesReps: IndexedSeq[AlignedEdge],
      dir: Direction,
      isOuterFace: Boolean = false,
  ): Seq[AlignedEdge] =
    val faceRep         = allFacesReps(face)
    val faceEdges       = traverseEdgesAlignedFace(faceRep).toSeq
    if (faceEdges.size == 0) then sys.error("Face of a rectangular dissection contains no edges!")
    val faceEdgesCyclic = faceEdges.appendedAll(faceEdges.drop(1))
    var i               = 0;
    while i < faceEdgesCyclic.length do
      while (
        i < faceEdgesCyclic.length &&
        !(faceEdgesCyclic(i).direction == dir && faceEdgesCyclic((i + 1) % faceEdgesCyclic.length).direction == (if !isOuterFace
                                                                                                                 then
                                                                                                                   dir.turnCW
                                                                                                                 else
                                                                                                                   dir.turnCCW
        ))
      ) do i = i + 1
      end while
      val removedHead = faceEdgesCyclic.drop(i + 1)
      val removedTail = removedHead.takeWhile(_.direction == (if !isOuterFace then dir.turnCW else dir.turnCCW))
      val tail        = removedHead.dropWhile(_.direction == (if !isOuterFace then dir.turnCW else dir.turnCCW))
      if tail.isEmpty then return Seq.empty
      if tail(0).direction == dir.reverse then return removedTail else i = i + 1
    end while
    return Seq.empty
  end getAllEdgesOfFaceInDirection

  def isCorner(e1: AlignedEdge, e2: AlignedEdge): Boolean =
    e1.direction != e2.direction && e1.to == e2.from
  end isCorner

  case class AlignedEdgeWFaces(e: AlignedEdge, neigborRight: Int, neigborLeft: Int);

  case class FaceWithEdgesPerDirection(
      var topEdges: Seq[AlignedEdge],
      var leftEdges: Seq[AlignedEdge],
      var bottomEdges: Seq[AlignedEdge],
      var rightEdges: Seq[AlignedEdge],
  ):
    def update(edges: Seq[AlignedEdge], dir: Direction) =
      dir match
        case Direction.North => topEdges = edges
        case Direction.East  => rightEdges = edges
        case Direction.South => bottomEdges = edges
        case Direction.West  => leftEdges = edges

    def getInDirection(dir: Direction): Seq[AlignedEdge] =
      dir match
        case Direction.North => topEdges
        case Direction.East  => rightEdges
        case Direction.South => bottomEdges
        case Direction.West  => leftEdges

  end FaceWithEdgesPerDirection

  object FaceWithEdgesPerDirection:
    def empty: FaceWithEdgesPerDirection =
      FaceWithEdgesPerDirection(Seq.empty, Seq.empty, Seq.empty, Seq.empty)

  def createFlowNetwork(
      dir: Direction = Direction.North,
  ): (DefaultDirectedGraph[Int, DefaultEdge], Map[DefaultEdge, Seq[AlignedEdge]]) =
    // create a node for each (inner) face
    val g = DefaultDirectedGraph[Int, DefaultEdge](classOf[DefaultEdge]);

    val faceReps = getOneEdgePerFace()

    // val innerFaceReps = faceReps.filter(f => !isOuterFace(f));
    val outerFaceIndex = faceReps.zipWithIndex.filter((f, _) => isOuterFace(f)).map((_, i) => i)

    if outerFaceIndex.isEmpty then sys.error("No outer face found!")

    for i <- 0 to faceReps.length do g.addVertex(i)
    end for

    val s = g.vertexSet().size()
    g.addVertex(s)

    val t = outerFaceIndex(0)

    // get map from edge to faces
    val edgeToFaceMap = getMapEdgeToAdjacentFace(faceReps)

    var faceToEdgesInDir = faceReps
      .map(_ => FaceWithEdgesPerDirection.empty) // mutable.IndexedBuffer[FaceWithEdgesPerDirection]().empty

    var flowEdgeToEdgeMap: mutable.Map[DefaultEdge, AlignedEdge] = mutable.Map.empty;

    var allFlowEdgesAndOriginal: Seq[(DefaultEdge, AlignedEdge)] = Seq.empty;

    // set for each face for each direction the bounding edges
    for i <- 0 to faceReps.length - 1 do
      for dir <- Direction.values.toSeq do
        val edgesInDirection = getAllEdgesOfFaceInDirection(i, faceReps.toIndexedSeq, dir, isOuterFace(faceReps(i)))
        val currentFace      = faceToEdgesInDir(i)
        currentFace.update(edgesInDirection, dir)
      end for
    end for
    // add directed edges to graph
    for i <- 0 to faceReps.length - 1 do
      if !isOuterFace(faceReps(i)) then
        val edgesInDirection                                                         = faceToEdgesInDir(i).getInDirection(dir)
        val allNeighboringFaces: Seq[(adjFace: Int, correspondingEdge: AlignedEdge)] = edgesInDirection
          .flatMap(e => (edgeToFaceMap.get(e).getOrElse(Seq.empty).map((_, e)))).filter((f, _) => f != i)
        // TODO: Es können eine oder mehrer Facetten benachbart sein. Falls nur eine Facette über
        // mehere Kanten adjazent sind, wird der Arc nur einmal hinzugefügt, für alle anderen gibt
        // das hinzufügen "null" zurück, weil die Kante bereits existiert => vorher groupBy adjazent Face,
        // dann den Arc hinzufügen und für alle gruppierten Kanten, den Arc setzen
        val allNeighboringFacesGroupedByFace                                         = allNeighboringFaces.groupBy(faceWithEdge => faceWithEdge.adjFace)
        val newArcWithAllEdges                                                       = allNeighboringFacesGroupedByFace
          .map(groupedfaceWithEdge => (g.addEdge(i, groupedfaceWithEdge._1), groupedfaceWithEdge._2)).toSeq
        val newFlowEdges                                                             = newArcWithAllEdges
          .flatMap((newArc, faceWithEdge) => faceWithEdge.map(e => (newArc, e.correspondingEdge)).toSeq)
        allFlowEdgesAndOriginal.++=(newFlowEdges);
    end for
    // connect s to the remaining graph
    val outerFace = faceToEdgesInDir(outerFaceIndex(0))
    val edgesInDirection                                                         = outerFace.getInDirection(dir.reverse)
    val allNeighboringFaces: Seq[(adjFace: Int, correspondingEdge: AlignedEdge)] = edgesInDirection
      .flatMap(e => (edgeToFaceMap.get(e).getOrElse(Seq.empty).map((_, e)))).filter((f, _) => f != s && f != t)
    val newFlowEdges                                                             = allNeighboringFaces
      .map(faceWithEdge => (g.addEdge(s, faceWithEdge.adjFace), faceWithEdge.correspondingEdge))
    allFlowEdgesAndOriginal.++=(newFlowEdges);
    // flow back
    g.addEdge(t, s)

    val allEdges = g.edgeSet.toArray().toSeq

    val mapFlowArcToEdge = allFlowEdgesAndOriginal.groupBy(arcAndEdge => arcAndEdge._1).map((k, v) => (k, v.map(_._2)))
      .toMap

    (g, mapFlowArcToEdge)

  end createFlowNetwork

  def solveFlowNetwork(network: org.jgrapht.Graph[Int, DefaultEdge]): Seq[(DefaultEdge, Double)] =

    val nodeDemand: java.util.function.Function[Int, Integer] = (_: Int) => 0

    val minArcCapacityFunc: java.util.function.Function[DefaultEdge, Integer] = (_: DefaultEdge) => 1

    val maxArcCapacityFunc: java.util.function.Function[DefaultEdge, Integer] = (_: DefaultEdge) =>
      CapacityScalingMinimumCostFlow.CAP_INF

    // val costFunc: java.util.function.Function[DefaultEdge, Double] = (_: DefaultEdge) => 1.0

    val problemInstance =
      MinimumCostFlowProblemImpl[Int, DefaultEdge](
        network,
        nodeDemand,
        maxArcCapacityFunc,
        minArcCapacityFunc,
        // costFunc,
      )

    val minCostFlow = CapacityScalingMinimumCostFlow[Int, DefaultEdge]()

    val flow = minCostFlow.getMinimumCostFlow(problemInstance)

    val totalWidth = flow.getFlow

    val keyMap = flow.getFlowMap;

    keyMap.entrySet().asScala.toSeq.map(entry => (entry.getKey(), entry.getValue()))

  end solveFlowNetwork

  def linksToEdges(startNode: NodeIndex, links: Seq[AlignedLink]): Seq[AlignedEdge] =
    if links.isEmpty then return Seq.empty
    val res = links.foldLeft(Seq(AlignedEdge(startNode, links(0).toNode, links(0).direction)))((acc, link) =>
      acc.appended(AlignedEdge(acc.last.to, link.toNode, link.direction)),
    )
    res
  end linksToEdges

  def determineEdgeLength(): Seq[AlignedWithLengthEdge] =
    val dissectedGraph = this.rectangularDissection();

    val (verticalFlowNetwork, verticalArcToEdgeMap) = dissectedGraph.createFlowNetwork(Direction.North)

    val (horizontalFlowNetwork, horizontalArcToEdgeMap) = dissectedGraph.createFlowNetwork(Direction.East)

    val horizontalArcLengths = solveFlowNetwork(verticalFlowNetwork)

    val verticalArcLegnths = solveFlowNetwork(horizontalFlowNetwork)

    val horEdgeLengths = horizontalArcLengths.filter((e, _) => verticalArcToEdgeMap.contains(e))
      .map((e, len) => AlignedWithLengthEdge().fromAlignedEdge(verticalArcToEdgeMap(e), len.toInt))

    val vertEdgeLengths = verticalArcLegnths.filter((e, _) => horizontalArcToEdgeMap.contains(e))
      .map((e, len) => AlignedWithLengthEdge().fromAlignedEdge(horizontalArcToEdgeMap(e), len.toInt))

    val dissectedEdgesWithLength = horEdgeLengths.++(vertEdgeLengths)

    val dissectedGraphWithLength = AlignedWithLengthGraph.fromAlignedWithLengthEdges(dissectedEdgesWithLength)
      .mkAlignedWithLengthGraph

    // val originalEdgesWithLength = dissectedGraphWithLength
    //  .filter(e => e._1.from.toInt < this.vertices.length && e._1.to.toInt < this.vertices.length)

    val originalEdgesWithLength = edges.map(e => dissectedGraphWithLength.getDissectedEdges(e))

    val accumulatedEdgeLengths = originalEdgesWithLength.map(_.map(_.length).reduce(_ + _)).zip(edges)
      .map((length, edge) => AlignedWithLengthEdge(edge.from, edge.to, edge.direction, length))

    accumulatedEdgeLengths

  end determineEdgeLength

  def positionsFromEdgeLength(): VertexLayout =

    val accumulatedEdgeLengths = determineEdgeLength();

    val graphWithEdgeLength = AlignedWithLengthGraph.fromAlignedWithLengthEdges(accumulatedEdgeLengths)
      .mkAlignedWithLengthGraph;

    val nodePositions = graphWithEdgeLength.getPositions();

    VertexLayout(nodePositions.toIndexedSeq)

  end positionsFromEdgeLength

end AGImpl
