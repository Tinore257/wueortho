package wueortho.data
import scala.collection.mutable
import scala.collection.AbstractIterator
import scala.collection.mutable.IndexedBuffer

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

  /** @param edge
    * @return
    */
  def getReverseEdge(edge: AlignedEdge): AlignedEdge

  /** @param edge
    * @return
    */
  def nodeAndReverseNode(edge: AlignedEdge): Seq[AlignedEdge]

  def splitEdge(
      edge: AlignedEdge,
      currentVertices: IndexedSeq[Vertex[AlignedLink]],
  ): (newNode: NodeIndex, newEdges: Seq[AlignedEdge], allNewEdges: Seq[AlignedEdge])

  /** @return
    */
  def getOneEdgePerFace(): mutable.IndexedBuffer[AlignedEdge]

  /** @param edges
    * @param changes
    * @return
    */
  def updateEdgesAfterSplit(
      edges: mutable.IndexedBuffer[AlignedEdge],
      changes: (newEdges: Seq[AlignedEdge], replacedEdges: Seq[AlignedEdge]),
  ): mutable.IndexedBuffer[AlignedEdge]

  /** Determinse for a given edge if this edge is part of an outer face
    * @param startEdge
    * @return
    */
  def isOuterFace(startEdge: AlignedEdge): Boolean

  /** @param face
    * @param allFacesReps
    * @param dir
    * @param isOuterFace
    * @return
    */
  def getAllEdgesOfFaceInDirection(
      face: Int,
      allFacesReps: IndexedSeq[AlignedEdge],
      dir: Direction,
      isOuterFace: Boolean = false,
  ): Seq[AlignedEdge]

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

  def nodeAndReverseNode(edge: AlignedEdge): Seq[AlignedEdge] =
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

  case class AlignedEdgeWFaces(e: AlignedEdge, neigborRight: Int, neigborLeft: Int);

  def linksToEdges(startNode: NodeIndex, links: Seq[AlignedLink]): Seq[AlignedEdge] =
    if links.isEmpty then return Seq.empty
    val res = links.foldLeft(Seq(AlignedEdge(startNode, links(0).toNode, links(0).direction)))((acc, link) =>
      acc.appended(AlignedEdge(acc.last.to, link.toNode, link.direction)),
    )
    res
  end linksToEdges

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
end AGImpl
