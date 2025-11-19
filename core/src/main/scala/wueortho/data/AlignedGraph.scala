package wueortho.data
import scala.collection.mutable
import scala.collection.AbstractIterator
import scala.collection.mutable.IndexedBuffer
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.alg.flow.mincost.CapacityScalingMinimumCostFlow
import org.jgrapht.alg.flow.mincost.MinimumCostFlowProblem.MinimumCostFlowProblemImpl
import scala.jdk.CollectionConverters.*
import org.jgrapht.graph.DirectedMultigraph

// reverseIndex: Position in der Adjazenzliste der toNode, von dem Link zur aktuellen fromNode
case class AlignedLink(toNode: NodeIndex, reverseIndex: Int, direction: Direction) derives CanEqual:
  def unalign = BasicLink(toNode, reverseIndex)

case class AlignedEdge(from: NodeIndex, to: NodeIndex, direction: Direction) derives CanEqual:
  def unalign = SimpleEdge(from, to)

case class SquareChain(chain: Seq[NodeIndex], chainComplement: Seq[NodeIndex], sigma: Seq[NodeIndex])

trait AlignedOps:

  /** creates a iterator that returns the next NodeIndex along the given face
    * @param start
    * @param direction
    * @param end
    * @param cw
    * @param cyclic
    * @return
    */
  def traverseAlignedFace(
      start: NodeIndex,
      direction: Direction,
      end: NodeIndex,
      cw: Boolean,
      cyclic: Boolean = false,
  ): Iterator[NodeIndex]

  /** creates a iterator that returns the next AlignedEdge along the given face
    * @param startEdge
    * @param cw
    * @param cyclic
    * @return
    */
  def traverseEdgesAlignedFace(
      startEdge: AlignedEdge,
      cw: Boolean,
      cyclic: Boolean = false,
  ): Iterator[AlignedEdge]

  /** Determinse for a given edge if this edge is part of an outer face
    * @param startEdge
    * @return
    */
  def isOuterFace(startEdge: AlignedEdge): Boolean

  /** Dissects a face given by a edge into rectangles
    * @param startNode
    * @param startEdge
    * @return
    */
  def rectangularDissectFace(
      startNode: NodeIndex,
      startEdge: AlignedEdge,
  ): (newEdges: Seq[AlignedEdge], replacedEdges: Seq[AlignedEdge])

  /** Dissects a connected AlignedGraph into rectangles
    * @return
    */
  def rectangularDissection(): AlignedGraph

  /** @param seq
    */
  def getBottomRightCorner(seq: Seq[AlignedEdge]): NodeIndex

  /** @param dir
    * @return
    */
  def createFlowNetwork(
      dir: Direction = Direction.North,
  ): (DirectedMultigraph[Int, DefaultEdge], Map[DefaultEdge, AlignedEdge])

  /** @param network
    * @return
    */
  def solveFlowNetwork(network: org.jgrapht.Graph[Int, DefaultEdge]): Seq[(DefaultEdge, Double)]

  /** @return
    */
  def determineEdgeLength(): Seq[AlignedWithLengthEdge]

  /** @return
    */
  def positionsFromEdgeLength(): VertexLayout

  /** @param origPos
    * @return
    */
  def planarize(origPos: VertexLayout): (AlignedGraph, VertexLayout)

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
      private var startEdge: Option[AlignedEdge]   = None;
      private var isFirstLink                      = true;
      private var currentFrom: Option[NodeIndex]   = None;
      private var currentLink: Option[AlignedLink] = None;
      private var counter                          = 0;

      def isStartLink(link: AlignedLink, fromNode: NodeIndex): Boolean =
        (startEdge.isDefined && currentFrom.isDefined) && (link.toNode == startEdge.get.to && fromNode == startEdge.get
          .from)
      def hasNext                                                      =
        (cyclic || (isFirstLink || !isFirstLink && currentLink.isDefined && (startEdge.isDefined && currentFrom
          .isDefined) && !(isStartLink(currentLink.get, currentFrom.get)))) && !nodes(current.toInt).neighbors.isEmpty
      def next(): AlignedLink                                          =
        startEdge match
          case Some(_) => isFirstLink = false
          case None    => ()

        def getFirstExistingDir(node: NodeIndex, startDir: Direction, nextDir: Direction => Direction): Direction =
          val currentDir = nextDir(startDir)
          if nodes(node.toInt).neighbors.map(l => l.direction).contains(currentDir) then return currentDir
          else getFirstExistingDir(node, currentDir, nextDir)

        val getNextDir = cw match
          case true  => Direction.turnCW
          case false => Direction.turnCCW

        val nextDir  = getFirstExistingDir(current, currentDirection.reverse, getNextDir)
        val nextLink = nodes(current.toInt).neighbors.filter(l => l.direction == nextDir).last
        startEdge match
          case Some(value) => ()
          case None        => startEdge = Some(AlignedEdge(current, nextLink.toNode, nextLink.direction))

        counter = counter + 1;
        if !cyclic && counter > 2 * (vertices.length * 3 - 6) then sys.error("The provided graph can not be planar!")
        currentLink = Some(nextLink)
        currentFrom = Some(current)
        current = nextLink.toNode
        currentDirection = nextLink.direction
        nextLink
      end next
    end new

  end traverseAlignedLinksFace

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
      val smallesFromNode      = face.minBy(_.from.toInt).from
      val allSmallestFromEdges = face.filter(_.from == smallesFromNode)
      val smallestToNode       = allSmallestFromEdges.minBy(_.to.toInt).to
      val smallestElementIndex = face.zipWithIndex.find((e, _) => e.from == smallesFromNode && e.to == smallestToNode)
        .get._2
      val sortedFace           = face.slice(smallestElementIndex, face.length).++(face.slice(0, smallestElementIndex))
      sortedFace
    end ensureSmallestEdgeFirst

    val unhandledEdges                                   = this.edges.to(IndexedBuffer)
    val faceEdgeCandidate: mutable.Set[Seq[AlignedEdge]] = mutable.Set().empty
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
    def addBoundingBox(edges: Seq[AlignedEdge], numberOfComponents: Int): Seq[AlignedEdge] =
      val i         = edges.flatMap(e => Seq(e.from, e.to)).map(_.toInt).max + 1
      val dockEdges = Range(0, numberOfComponents)
        .map(index => AlignedEdge(NodeIndex(i + index), NodeIndex(i + index + 1), Direction.West))
      val newEdges  = dockEdges.++(
        Seq(
          AlignedEdge(NodeIndex(i + 2), NodeIndex(i + 3), Direction.North),
          AlignedEdge(NodeIndex(i + 3), NodeIndex(i + 4), Direction.East),
          AlignedEdge(NodeIndex(i + 4), NodeIndex(i), Direction.South),
        ),
      )
      newEdges
    end addBoundingBox
    var faceEdgeCandidate                                                                  = getOneEdgePerFace()
    var allEdges                                                                           = this.edges.toSet
    var uncheckedNodes                                                                     = Range(0, nodes.length).map(NodeIndex(_)).toSet
    val allComponents: mutable.Set[Set[NodeIndex]]                                         = mutable.Set.empty
    while !uncheckedNodes.isEmpty do
      val startNode = uncheckedNodes.toSeq(0)
      val adj       = (v: NodeIndex) => vertices(v.toInt).neighbors.map(_.toNode)
      val comp      = bfsTraverse(adj, startNode).toSet
      uncheckedNodes = uncheckedNodes.--(comp)
      allComponents.+=(comp)
    end while
    val numberOfComponents                                                                 = allComponents.size
    val bb                                                                                 = addBoundingBox(allEdges.toSeq, numberOfComponents)
    allEdges.++=(bb)
    var currentGraph                                                                       = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
    var i                                                                                  = 0
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
        // TODO: For debugging
        // val x                    = edgesAfterCompaction.newEdges.flatMap(e => Seq(e, getReverseEdge(e)))
        //  .map(e => (e, compactedGraph.isOuterFace(e)))
        e =
          if edgesAfterCompaction.newEdges.isEmpty then e
          else edgesAfterCompaction.newEdges.find(e => compactedGraph.isOuterFace(e)).get
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
  ): (DirectedMultigraph[Int, DefaultEdge], Map[DefaultEdge, AlignedEdge]) =
    // create a node for each (inner) face
    val g = DirectedMultigraph[Int, DefaultEdge](classOf[DefaultEdge]);

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

    val faceToEdgesInDir = faceReps
      .map(_ => FaceWithEdgesPerDirection.empty) // mutable.IndexedBuffer[FaceWithEdgesPerDirection]().empty

    val flowEdgeToEdgeMap: mutable.Map[DefaultEdge, AlignedEdge] = mutable.Map.empty;

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
        val newFlowEdges                                                             = allNeighboringFaces
          .map(faceWithEdge => (g.addEdge(i, faceWithEdge.adjFace), faceWithEdge.correspondingEdge)).toSeq
        // val newFlowEdges                                                             = newArcWithAllEdges
        //  .flatMap((newArc, faceWithEdge) => faceWithEdge.map(e => (newArc, e.correspondingEdge)).toSeq)
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

    val mapFlowArcToEdge = allFlowEdgesAndOriginal.toMap

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

  def planarize(origPos: VertexLayout): (AlignedGraph, VertexLayout) =

    var pos = origPos

    def splitEdgeIntersection(
        e1: AlignedEdge,
        e2: AlignedEdge,
        pos: VertexLayout,
        crossNodeIndex: NodeIndex,
    ): (newEdges: Seq[AlignedEdge], newPos: Vec2D) =
      /** creates new edges and node based on intersection of two edges
        *
        * @param edge
        * @param crossNodeIndex
        * @return
        */
      def split(edge: AlignedEdge, crossNodeIndex: NodeIndex): Seq[AlignedEdge] =
        Seq(
          AlignedEdge(edge.from, crossNodeIndex, edge.direction),
          AlignedEdge(crossNodeIndex, edge.to, edge.direction),
        )
      end split

      def getIntersectionPoint(e1: AlignedEdge, e2: AlignedEdge, pos: VertexLayout): Vec2D =
        // based on https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
        val vec2DToTuple         = (p: Vec2D) => (p.x1, p.x2)
        val ((x1, y1), (x2, y2)) = (vec2DToTuple(pos(e1.from)), vec2DToTuple(pos(e1.to)))
        val ((x3, y3), (x4, y4)) = (vec2DToTuple(pos(e2.from)), vec2DToTuple(pos(e2.to)))
        val dx12                 = x1 - x2
        val dx34                 = x3 - x4
        val dy12                 = y1 - y2
        val dy34                 = y3 - y4
        val det12                = x1 * y2 - y1 * x2
        val det34                = x3 * y4 - y3 * x4
        val px                   = ((det12 * dx34) - (dx12 * det34)) / ((dx12 * dy34) - (dy12 * dx34))
        val py                   = ((det12 * dy34) - (dy12 * det34)) / ((dx12 * dy34) - (dy12 * dx34))
        Vec2D(px, py)
      end getIntersectionPoint

      if !IntersectionTools().intersect(e1, e2, pos) then sys.error("Edges do not intersect")
      val newEdges = split(e1, crossNodeIndex).++(split(e2, crossNodeIndex))
      val point    = getIntersectionPoint(e1, e2, pos)
      (newEdges, point)
    end splitEdgeIntersection

    var removedEdges: Set[AlignedEdge]          = Set.empty
    val edgesBuffer: IndexedBuffer[AlignedEdge] = edges.to(IndexedBuffer)
    var nextNodeIndex                           = vertices.length

    var i           = 0
    while i < edgesBuffer.length do
      val e1 = edgesBuffer(i)
      if !(removedEdges.contains(e1) || removedEdges.contains(getReverseEdge(e1))) then
        var j = (i + 1)
        while j < edgesBuffer.length do
          val e2 = edgesBuffer(j)
          if !(removedEdges.contains(e2) || removedEdges.contains(getReverseEdge(e2))) then
            if IntersectionTools().intersect(e1, e2, pos) then
              val (splittedEdges, newPos) = splitEdgeIntersection(e1, e2, pos, NodeIndex(nextNodeIndex))
              nextNodeIndex = nextNodeIndex + 1
              pos = VertexLayout(pos.nodes.appended(newPos))
              edgesBuffer ++= (splittedEdges)
              removedEdges ++= (Set(e1, e2))
              j = edgesBuffer.length
          j = j + 1
        end while
      end if
      i = i + 1
    end while
    // TODO: Add edges based on disjoint sets
    val newEdges    = edgesBuffer.toSet.--(removedEdges.flatMap(e => Seq(e, getReverseEdge(e))))
    val planarGraph = AlignedGraph.fromAlignedEdges(newEdges.toSeq).mkAlignedGraph
    print("Positions after planarisation: {}", pos.toString())
    (planarGraph, pos)
  end planarize

  // ###################### Start BFS ############################################
  private def bfsTraverse(neighbors: NodeIndex => Seq[NodeIndex], start: NodeIndex) =
    val visited = mutable.BitSet.empty
    val result  = mutable.ArrayBuffer.empty[NodeIndex]
    val queue   = mutable.ArrayDeque(start)

    while queue.nonEmpty do
      val next = queue.removeHead()
      if !visited(next.toInt) then
        result += next
        visited += next.toInt
        for node <- neighbors(next) if !visited(node.toInt) do queue += node
    end while

    result.toSeq
  end bfsTraverse
  // ############################ END BFS ###########################################

end AGImpl
