// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout}
import wueortho.data.Direction
import scala.collection.mutable
import wueortho.util.mutable.DisjointSets
import wueortho.util.Monoid
import wueortho.data.NodeIndex
import wueortho.data.SimpleEdge
import wueortho.data.VertexBoxes
import wueortho.data.AlignedEdge
import wueortho.data.AlignedGraph
import wueortho.data.BasicGraph
import wueortho.util.GraphConversions.sg2dg
import wueortho.data.IntersectionTools
import wueortho.data.Graph
import wueortho.data.AlignedLink
import wueortho.data.BasicLink
import org.jgrapht.graph.SimpleGraph
import scala.collection.AbstractIterator
import wueortho.metrics.Crossings.interEdgeDist
import org.jgrapht.event.EdgeTraversalEvent
extension (a: Vec2D)
  def *(scale: Double): Vec2D =
    Vec2D(a.x1 * scale, a.x2 * scale)

object GreedyOrthogonalization:

  @main 
  def testMain() =
    //val aEdges = Seq(AlignedEdge(NodeIndex(0), NodeIndex(1), Direction.North), AlignedEdge(NodeIndex(0), NodeIndex(4), Direction.East))
    //val aGraph = AlignedGraph.fromAlignedEdges(aEdges).mkAlignedGraph
    //val allEdges = Seq((0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)).map(e => SimpleEdge(NodeIndex(e._1), NodeIndex(e._2)))
    //val graph = Graph.fromEdges(allEdges).mkBasicGraph
    //val posSeq = IndexedSeq(Vec2D(0, 0), Vec2D(0, 3), Vec2D(2, 3), Vec2D(3, 2), Vec2D(3, 0), Vec2D(3, 3), Vec2D(3, -1), Vec2D(-2, 3))
    //val pos = VertexLayout(posSeq)
    //val unaligendNeighbors = getUnalignedOfQuadrant(graph, NodeIndex(0), 1, Option.empty, Option.empty, pos)
    //val s = splitAlignedEdge(graph, aGraph, aEdges(0), pos, 0.001)
    //val unalignedEdges = unaligendNeighbors.map(v => SimpleEdge(NodeIndex(0), v))
    //val a = alignUnalignedEdges(graph, aGraph, pos, aEdges(0), unalignedEdges)
    //val b = alignAllNeighbors(graph, aGraph, pos, NodeIndex(0))

    val allEdges2 = Seq((0, 1), (0, 3), (1, 2),(1, 8), (1, 9), (2, 3), (2, 4), (2, 6), (3, 4), (4, 5), (5, 6), (6, 7), (7,  8), (8, 9)).map(e => SimpleEdge(NodeIndex(e._1), NodeIndex(e._2)))
    val posSeq2 = IndexedSeq(Vec2D(0, 0), Vec2D(0, 3), Vec2D(4, 3), Vec2D(4, 0), Vec2D(6, 2), Vec2D(7, 5), Vec2D(4, 6), Vec2D(2, 5), Vec2D(0, 6), Vec2D(-2, 5))
    val pos2 = VertexLayout(posSeq2)
    val graph2 = Graph.fromEdges(allEdges2).mkBasicGraph

    //val c = allignAllUnalignedEdges(graph, aGraph,pos)
    //val d = IntersectionTools().intersect(allEdges2(0), allEdges2(1), pos2)
    val e = traverseFace(graph2, pos2, SimpleEdge(NodeIndex(1), NodeIndex(2)), false).toSeq

    val x = 0;
  end testMain


  def topologicalSort(ids: Seq[NodeIndex], neighbors: NodeIndex => Seq[NodeIndex]): Seq[NodeIndex] =
    val visited  = mutable.BitSet.empty
    val finished = mutable.BitSet.empty
    val result   = mutable.ArrayBuffer.empty[NodeIndex]

    var cycleFlag = false

    def visit(v: NodeIndex): Unit =
      if !cycleFlag && !finished.contains(v.toInt) then
        if visited.contains(v.toInt) then cycleFlag = true
        val _ = visited.add(v.toInt)
        for u <- neighbors(v) do visit(u)
        val _ = finished.add(v.toInt)
        result.append(v)

    // get nodes without ingoing edges
    val verticesWithIngoingEdges = ids.flatMap(id => neighbors(id))

    for v <- ids do
      if !verticesWithIngoingEdges.contains(v) then visit(v)
      if cycleFlag then sys.error("no topological ordering because of cycles!")

    return result.toSeq
  end topologicalSort

  def getAngle(reference:NodeIndex, n: NodeIndex, pos: VertexLayout) =
    math.atan2(pos(n).x2 - pos(reference).x2, pos(n).x1 - pos(reference).x1) 
  end getAngle

  /**
    * sorts the neighbors based on their angle relativ to node 
    *
    * @param node the reference point
    * @param neighbors points to sort
    * @param pos position vector of all points
    * @param ccw 
    * @return Tuple of neighbors NodeIndex and corresponding angle
    */
  def getRadialOrdering(node: NodeIndex, neighbors: Seq[NodeIndex], pos: VertexLayout, ccw: Boolean = true): IndexedSeq[(NodeIndex, Double)] =
    val ordering = neighbors.toIndexedSeq.map(n => (n, getAngle(node, n, pos))).sortBy(_._2)
    if ccw then ordering else ordering.reverse 
  end getRadialOrdering 

  /**
    * sorts the edges to the neighbors based on their angle relativ to node 
    *
    * @param node the refrence point
    * @param neighborsEdges edges to sort
    * @param pos position vector of all points
    * @param ccw
    * @return Tuple of edges and corresponding angle
    */
   def getRadialOrderingNeighbors(node: NodeIndex, neighborsEdges: Seq[SimpleEdge], pos: VertexLayout, ccw: Boolean = true): IndexedSeq[(SimpleEdge, Double)] =
    val ordering = neighborsEdges.toIndexedSeq.map(n => (n, getAngle(node, n.to, pos))).sortBy(_._2)
    if ccw then ordering else ordering.reverse 
  end getRadialOrderingNeighbors 


  def bothSimpleEdges(from: NodeIndex, to: NodeIndex): Seq[SimpleEdge] = 
    Seq(SimpleEdge(from, to), SimpleEdge(to, from))
  end bothSimpleEdges

  def bothAlignedEdges(e: AlignedEdge): Seq[AlignedEdge] = 
    Seq(AlignedEdge(e.from, e.to, e.direction), AlignedEdge(e.to, e.from, e.direction.reverse))
  end bothAlignedEdges


  /**
    * creates a new Graph such that the given aligned edge is splitted into two edges
    *
    * @param graph
    * @param edge to split
    * @param pos positions of all nodes
    * @param positionFactor scaling factor to position new node
    * @return
    */
  def splitAlignedEdge(graph: BasicGraph, alignedGraph: AlignedGraph, edge: AlignedEdge, pos: VertexLayout, positionFactor: Double): (BasicGraph, AlignedGraph, VertexLayout, NodeIndex) = 
    val (aEdgesToRemove, uEdgesToRemove) = (bothAlignedEdges(edge), bothSimpleEdges(edge.from, edge.to))
    val withoutEdge = graph.edges.toSet--(uEdgesToRemove)
    val iPos = ((pos(edge.to) - pos(edge.from)) * positionFactor) + pos(edge.from)
    val newNodeIndex = NodeIndex(graph.vertices.length)
    val additionalEdges: Seq[AlignedEdge] = Seq(AlignedEdge(edge.from, newNodeIndex, edge.direction), AlignedEdge(newNodeIndex, edge.to, edge.direction))
    val newGraph = Graph.fromEdges(withoutEdge.++(additionalEdges.map(_.unalign)).toSeq).mkBasicGraph
    val newAGraph = AlignedGraph.fromAlignedEdges(alignedGraph.edges.toSet.--(aEdgesToRemove).++(additionalEdges).toSeq).mkAlignedGraph
    (newGraph, newAGraph, VertexLayout(pos._1.:+(iPos)), newNodeIndex)
  end splitAlignedEdge

  def flipEdgeToAlignendEdge(graph: BasicGraph, alignedGraph: AlignedGraph, oldStart: NodeIndex, newStart:NodeIndex, node: NodeIndex, dir: Direction): (graph: BasicGraph, alignedGraph: AlignedGraph) = 
    val uEdgesToRemove = bothSimpleEdges(oldStart, node)
    val aNewEdge = AlignedEdge(newStart, node, dir)
    val newGraph = Graph.fromEdges(graph.edges.toSet.--(uEdgesToRemove).+(aNewEdge.unalign).toSeq).mkBasicGraph
    val newAGraph = AlignedGraph.fromAlignedEdges(alignedGraph.edges.appended(aNewEdge)).mkAlignedGraph
    (newGraph, newAGraph)
  end flipEdgeToAlignendEdge
    
  def hasNoConflicitingAlignment(neighbor: NodeIndex, alignedGraph: AlignedGraph, direction: Direction): Boolean = 
    alignedGraph.vertices.size <= neighbor.toInt || !alignedGraph.vertices(neighbor.toInt).neighbors.exists(_.direction == direction)
  end hasNoConflicitingAlignment

  def alignUnalignedEdges(graph: BasicGraph, alignedGraph: AlignedGraph, pos: VertexLayout, edgeToSplit: AlignedEdge, unalignedEdges: IndexedSeq[SimpleEdge]): (BasicGraph, AlignedGraph, VertexLayout) =  
    // sorted smallest angle to edgeToSplit to highest
    val sortedNeighborsEdges = getRadialOrderingNeighbors(edgeToSplit.from, unalignedEdges, pos, false)
    val sortedNeighbors = sortedNeighborsEdges.map(_._1).map(e => if e.from == edgeToSplit.from then e.to else e.from)
    var currentEdgeToSplit = edgeToSplit
    var status : (graph: BasicGraph, alignedGraph: AlignedGraph, pos: VertexLayout, topNode: NodeIndex) =  (graph, alignedGraph, pos, edgeToSplit.to)
    var totalDocks = sortedNeighbors.filter(neighbor => hasNoConflicitingAlignment(neighbor, alignedGraph, edgeToSplit.direction.turnCCW)).size + 1    
    var factor = 0.01 
    for neighbor <- sortedNeighbors do
      if hasNoConflicitingAlignment(neighbor, alignedGraph, edgeToSplit.direction.turnCCW) then
        status = splitAlignedEdge(status.graph, status.alignedGraph, currentEdgeToSplit, status.pos, factor)
        currentEdgeToSplit = AlignedEdge(edgeToSplit.from, status.topNode, edgeToSplit.direction)
        //add edge from neighbor to node
        val (newGraph, newAGraph) = flipEdgeToAlignendEdge(status.graph, status.alignedGraph, edgeToSplit.from,  status.topNode, neighbor, edgeToSplit.direction.turnCW)
        status = (newGraph, newAGraph, status.pos, status.topNode)
        factor = 1.0-(1.0/totalDocks)
        totalDocks = totalDocks - 1
      end if
    end for

    (status.graph, status.alignedGraph, status.pos)
  end alignUnalignedEdges

  /**
    * clips the (sorted) list of edges between low (start) and high (end)
    *
    * @param edges
    * @param low
    * @param high
    * @return
    */
  def getEdgeInterval(edges: IndexedSeq[SimpleEdge], low: Option[SimpleEdge], high:Option[SimpleEdge]): IndexedSeq[SimpleEdge] =
    if low.isEmpty && high.isEmpty then return edges
    val cyclic = edges.++(edges)
    val suffix = low match
      case Some(value) => cyclic.dropWhile(_.to != value.to).drop(1)
      case None => edges
    val prefix = high match
      case Some(value) => suffix.takeWhile(_.to != value.to)
      case None => suffix.takeWhile(_.to != low.get.to)
    prefix
  end getEdgeInterval

  /**
    * returns all unaligned Neigbors of the referenceNode in the given quadrant. 
    * if start and/or end edges are given, the quadrants are extended up to those edges
    *
    * @param graph
    * @param referenceNode
    * @param quadrant
    * @param start
    * @param stop
    * @param pos
    * @return
    */
  def getUnalignedOfQuadrant(graph: BasicGraph, referenceNode: NodeIndex, quadrant: Int, start: Option[SimpleEdge], stop: Option[SimpleEdge], pos: VertexLayout): IndexedSeq[NodeIndex] = 
    val edges = graph.vertices(referenceNode.toInt).neighbors.map(n => SimpleEdge(referenceNode, n.toNode))
    def quadrantToAngles(quadrant: Int) = 
      val list = IndexedSeq(0, 1, 1, 2, -2, -1, -1,  0).map(_ * Math.PI / 2.0)
      val sublist = list.sliding(2, 2).toIndexedSeq((quadrant - 1) % 4)
      (sublist(0), sublist(1)):(low: Double, high: Double)
    val bounds = quadrantToAngles(quadrant)
    //val sortedEdges = getRadialOrderingNeigbors(referenceNode, edges.toSeq, pos).filter(n => n._2 >= quadrantToAngles(quadrant).low && n._2 < quadrantToAngles(quadrant).high ).map(_._1)
    val sortedWithAngle = getRadialOrderingNeighbors(referenceNode, edges.toSeq, pos)
    val clipped = (start, stop) match
      case (Some(_), Some(_)) => sortedWithAngle
      case (Some(_), None) => sortedWithAngle.filter(e => e._2 < bounds.high)
      case (None, Some(_)) => sortedWithAngle.filter(e => e._2 >= bounds.low)
      case (None, None) => sortedWithAngle.filter(e => e._2 >= bounds.low && e._2 < bounds.high)
    val sortedEdges = clipped.map(_._1)
    val betweenAlignedEdges = getEdgeInterval(sortedEdges, start, stop)

    betweenAlignedEdges.map(_.to)
  end getUnalignedOfQuadrant

  def getReverseEdge(e: SimpleEdge): SimpleEdge = 
    SimpleEdge(e.to, e.from)
  end getReverseEdge

  def aLink2Edge(startNode: NodeIndex, l: AlignedLink): SimpleEdge =
    SimpleEdge(startNode, l.toNode) 

  def link2Edge(startNode: NodeIndex, l: BasicLink): SimpleEdge =
    SimpleEdge(startNode, l.toNode)  

  def alignAllNeighbors(graph: BasicGraph, alignedGraph: AlignedGraph , pos: VertexLayout, node: NodeIndex): (BasicGraph, AlignedGraph, VertexLayout) =
    def borderDirsFromQuadrant(quadrant: Int): (Direction, Direction) =
      val list = IndexedSeq(Direction.East, Direction.North, Direction.West, Direction.South, Direction.East)
      val border = list.sliding(2).toIndexedSeq((quadrant - 1)%4)
      (border(0), border(1))
    end borderDirsFromQuadrant
    
    val unaligendEdges = graph.vertices(node.toInt).neighbors.filter(l => !alignedGraph.vertices(node.toInt).neighbors.exists(a => a.toNode == l.toNode))
    val aligendEdges = alignedGraph.vertices(node.toInt).neighbors

    var status: (currentGraph: BasicGraph, currentAlignedGraph: AlignedGraph, currentPos: VertexLayout) = (graph, alignedGraph,pos)

    for quadrant <- Range(1, 5) do
      val dirsForCurrentQuadrant = borderDirsFromQuadrant(quadrant)
      //helper functions
      val aToEdge = aLink2Edge(node, _)
      val toEdge = link2Edge(node,_)
      
      //bounds for interval
      val (low, high) = (aligendEdges.find(_.direction == dirsForCurrentQuadrant._1), aligendEdges.find(_.direction == dirsForCurrentQuadrant._2))
      
      if high.isDefined then 
        val unalignedInQuadrant = getUnalignedOfQuadrant(graph, node, quadrant, low.map(aToEdge), high.map(aToEdge),pos).map(SimpleEdge(node, _))
        //val (edgeToAlignTo, alignmentDir) = if high.isDefined then (high, high.get.direction.turnCCW) else if low.isDefined then (low, low.get.direction.turnCW) else (Option.empty, Direction.North)
        val (edgeToAlignTo, alignmentDir) = if high.isDefined then (high, high.get.direction.turnCCW) else (Option.empty, Direction.North)
        if edgeToAlignTo.isDefined then 
          val unalignedWithoutConflicts = unalignedInQuadrant.filter(e => hasNoConflicitingAlignment(e.to, alignedGraph, alignmentDir))
          status = alignUnalignedEdges(status.currentGraph, status.currentAlignedGraph, status.currentPos, AlignedEdge(node, edgeToAlignTo.get.toNode, edgeToAlignTo.get.direction), unalignedWithoutConflicts)
        end if
      end if
    end for
    status
  end alignAllNeighbors
  
  def allignAllUnalignedEdges(graph: BasicGraph, aligendGraph: AlignedGraph, pos: VertexLayout): (BasicGraph, AlignedGraph, VertexLayout) =
    val unalignedEdges = graph.edges.toSet.--(aligendGraph.edges.flatMap(e => Seq(e.unalign, getReverseEdge(e.unalign))))
    var status: (currentGraph: BasicGraph, currentAlignedGraph: AlignedGraph, currentPos: VertexLayout) = (graph, aligendGraph, pos)
    val nodesWithUnalignedEdges = unalignedEdges.flatMap(e => Seq(e.from, e.to)).toIndexedSeq.sortBy(v => graph.vertices(v.toInt).neighbors.length).reverse
    for node <- nodesWithUnalignedEdges do
      status = alignAllNeighbors(status.currentGraph, status.currentAlignedGraph, status.currentPos, node)
    end for
    status
  end allignAllUnalignedEdges

  case class EdgeIntersectionWithPos(e: SimpleEdge, pos: Vec2D)

  def getAllRayIntersections(graph: BasicGraph, pos: VertexLayout, start: NodeIndex, edgesToIgnore: Seq[SimpleEdge] = Seq.empty, dir: Vec2D = Vec2D(0, 1)): Seq[EdgeIntersectionWithPos] =
    val maxPos = pos.nodes.map(p => p.x1 max p.x2).max * 2.0
    val rayEndPos = Vec2D(dir.x1 * maxPos, dir.x2 * maxPos) + pos(start)
    val posWithRay = VertexLayout(pos.nodes.appended(rayEndPos))
    val rayEdge = SimpleEdge(start, NodeIndex(graph.vertices.size))
    var intersections: Seq[EdgeIntersectionWithPos] = Seq.empty
    for e <- graph.edges do
      if IntersectionTools().intersect(rayEdge, e, posWithRay) then 
        val point = IntersectionTools().getIntersectionPoint(rayEdge, e, posWithRay)
        intersections = intersections.appended(EdgeIntersectionWithPos(e, point))
      end if 
    end for
    intersections.filter(i => !edgesToIgnore.contains(i.e))
  end getAllRayIntersections
  
  def getClosestIntersection(start: NodeIndex, pos: VertexLayout, intersections: Seq[EdgeIntersectionWithPos]): Option[EdgeIntersectionWithPos] = 
    val getSquaredDist: (Vec2D) => Double = (other: Vec2D) =>
      val deltaDist = pos(start) - other
      deltaDist.x1 * deltaDist.x1 + deltaDist.x2 * deltaDist.x2
    if (intersections.isEmpty) then return None
    Some(intersections.sortBy(i => getSquaredDist(i.pos)).head)
  end getClosestIntersection

  def testContainmentInFace(graph: BasicGraph, pos: VertexLayout, start: NodeIndex): Seq[SimpleEdge] = 
    val allIntersections = getAllRayIntersections(graph, pos, start)
    if allIntersections.isEmpty then return Seq.empty
    val closestIntersection = getClosestIntersection(start, pos, allIntersections) 
    val (faceA, faceB) = (traverseFace(graph, pos, closestIntersection.get.e, true).toIndexedSeq, traverseFace(graph, pos, getReverseEdge(closestIntersection.get.e), true).toIndexedSeq) 
    val (faceAIntersectedEdges, faceBIntersectedEdges) = (allIntersections.filter(i => faceA.contains(i.e) || faceA.contains(getReverseEdge(i.e))), allIntersections.filter(i => faceB.contains(i.e) || faceB.contains(getReverseEdge(i.e))))
    
    val posWithRay = VertexLayout(pos.nodes.appended(Vec2D(0, 1)))
    val rayEdge = SimpleEdge(start, NodeIndex(graph.vertices.size))
    
    //Do more precise counting/ special cases
    def getCriticalIntersections(faceIntersectedEdegs: Seq[EdgeIntersectionWithPos]):Int = 
      val criticalEdges = faceIntersectedEdegs.filter(e => IntersectionTools().throughNodeTest(rayEdge, e.e.from, posWithRay) || IntersectionTools().throughNodeTest(rayEdge, e.e.to, posWithRay))
      val prefix = criticalEdges.takeWhile(c => IntersectionTools().throughNodeTest(rayEdge, c.e.from, posWithRay))
      val criticalWrapped = criticalEdges.drop(prefix.size).appendedAll(prefix)
      // to start a critical sequence there must be a edge, where the start node is not colinear to the ray
      val criticalSequeces = criticalWrapped.foldLeft(Seq.empty[Seq[EdgeIntersectionWithPos]])((acc, x) => (acc, x) match
        case (Seq(), x) => Seq(Seq(x))
        case (acc, x) if !IntersectionTools().throughNodeTest(rayEdge, x.e.from, posWithRay) => acc :+ Seq(x) 
        case (Seq(_*), x) => acc.init:+(acc.last:+x)
      )
      val filteredSequences = criticalSequeces.filter(sequence => IntersectionTools().orientationTest(rayEdge, sequence.head.e.from, posWithRay) != IntersectionTools().orientationTest(rayEdge, sequence.last.e.to, posWithRay))
      val simpleIntersections = faceIntersectedEdegs.filter(e => !IntersectionTools().throughNodeTest(rayEdge, e.e.from, posWithRay) && !IntersectionTools().throughNodeTest(rayEdge, e.e.to, posWithRay))
      return filteredSequences.size + simpleIntersections.size
    end getCriticalIntersections
    
    val (intersectionsA, intersectionsB) = (getCriticalIntersections(faceAIntersectedEdges), getCriticalIntersections(faceBIntersectedEdges)) 

    if intersectionsA % 2 == 1 then return faceA
    if intersectionsB % 2 == 1 then return faceB
    Seq.empty
  end testContainmentInFace

  def traverseFace(
      graph: BasicGraph, 
      pos: VertexLayout,
      start: SimpleEdge,
      cw: Boolean,
      cyclic: Boolean = false,
  ): Iterator[SimpleEdge] = 
    new AbstractIterator[SimpleEdge] {
      var currentEdge: SimpleEdge = start
      var stopEdge: Option[SimpleEdge] = None
      override def next(): SimpleEdge = 
        val ToEdge = link2Edge(currentEdge.to,_)
        val neighbors = graph.vertices(currentEdge.to.toInt).neighbors.map(ToEdge);
        val radialSortedNeighbors = getRadialOrderingNeighbors(currentEdge.to, neighbors, pos, cw).map(_._1)
        val incommingEdge = radialSortedNeighbors.indexOf(getReverseEdge(currentEdge))
        if incommingEdge == -1 then sys.error("incomming edge not found while traversing face!")
        stopEdge = Some(start)
        currentEdge = radialSortedNeighbors((incommingEdge + 1) % radialSortedNeighbors.size)
        currentEdge
      end next

      override def hasNext: Boolean = 
        !graph.vertices(currentEdge.to.toInt).neighbors.isEmpty && ( cyclic || (stopEdge.isEmpty || currentEdge != stopEdge.get))
      end hasNext

    }

  end traverseFace



  def greedyAlignedGraph(graph: BasicGraph, init: VertexLayout, boxes: VertexBoxes): AlignedGraph =
    val n = graph.numberOfVertices

    class PosVec(init: Seq[Vec2D]):
      var a = Array[Vec2D](init*)

      def apply(i: Int) = a(i)
      def finish        = a.toVector

      def update(i: Int, value: Vec2D) = a(i) = value

    end PosVec

    val pos = PosVec(init.nodes);

    if n < 2 then return AlignedGraph.fromAlignedEdges(Seq.empty).mkAlignedGraph

    given Monoid[Set[NodeIndex]]:
      def zero: Set[NodeIndex]                                                 = Set.empty
      override def apply(a: Set[NodeIndex], b: Set[NodeIndex]): Set[NodeIndex] = a union b

    val verticalSets   = DisjointSets[NodeIndex, Set[NodeIndex]]
    val horizontalSets = DisjointSets[NodeIndex, Set[NodeIndex]]

    var alingedEdges: Set[AlignedEdge] = Set.empty;

    Range(0, graph.numberOfVertices).foreach(i => verticalSets.mkSet(NodeIndex(i), Set(NodeIndex(i))))
    Range(0, graph.numberOfVertices).foreach(i => horizontalSets.mkSet(NodeIndex(i), Set(NodeIndex(i))))

    val assignments: IndexedSeq[mutable.IndexedSeq[(Direction, (Int, Int))]] = IndexedSeq(
      init.nodes.map(_ => Direction.values.map(d => (d, (-1, -1))).sortBy((d, _) => d.ordinal))*,
    )

    val allEdgeAngles = wueortho.data.mutable.Matrix.fill(n, n)(0)

    def angleOfEdge(e1: Vec2D, e2: Vec2D) =
      math.atan2((e2.x2 - e1.x2), (e2.x1 - e1.x1))

    // TODO: assumes, that graph is undirected weighted graph
    val undirectedGraph = sg2dg(graph)

    val allEdges = undirectedGraph.edges

    // precalculate all edge angles
    allEdges.map(b => (b.from, b.to, pos(b.from.toInt), pos(b.to.toInt)))
      .foreach((from, to, e1, e2) => allEdgeAngles(from.toInt, to.toInt) = angleOfEdge(e1, e2))

    def directionToAngle(dir: Direction) = dir match
      case Direction.East  => 0.0
      case Direction.North => Math.PI / 2
      case Direction.West  => Math.PI
      case Direction.South => -Math.PI / 2

    def edgeAlignmentCost(angle: Double, dir: Direction) =
      val semiAxisAngle = directionToAngle(dir)
      Math.abs(angle - semiAxisAngle) min Math.abs(angle - Math.PI * 2 - semiAxisAngle)


    val verticesOrderedByDegree = undirectedGraph.vertices.zipWithIndex.map((v, i) => (i, v.neighbors.length))
      .sortBy((_, l) => l).reverse

    def isAligned(e: SimpleEdge, dir: Direction) = dir match
      case Direction.East | Direction.West   =>
        // horizontalSets.contains(e.from) && horizontalSets.contains(e.to)
        (horizontalSets.contains(e.from) && horizontalSets.getOrElseThrow(e.from).contains(e.to))
      case Direction.North | Direction.South => // verticalSets.contains(e.from) && verticalSets.contains(e.to)
        (verticalSets.contains(e.from) && verticalSets.getOrElseThrow(e.from).contains(e.to))

    case class Candidate(dir: Direction, weight: Double, edge: (Int, Int))

    for (vertex, _) <- verticesOrderedByDegree do
      // calculate cost function for all edges starting at v
      val minCosts: mutable.IndexedSeq[Candidate] = Direction.values
        .map(d => Candidate(d, Double.PositiveInfinity, (0, 0))).sortBy(_.dir.ordinal)

      // get for each direction edge within 45 degrees with minimal costs
      for neighbor <- undirectedGraph.vertices(vertex).neighbors.map(v => v.toInt) do
        for dir <- Direction.values do
          val cost = edgeAlignmentCost(allEdgeAngles(vertex, neighbor), dir)
          if minCosts(dir.ordinal)._2 > cost && math.abs(cost) < Math.PI / 4
          then minCosts(dir.ordinal) = Candidate(dir, cost, (vertex, neighbor))

      // check for conflicts (i.e. double assignments, overrides) and maintain disjoint sets
      val localAssignments: mutable.IndexedSeq[(Direction, (Int, Int))] = Direction.values.map(d => (d, (-1, -1)))
        .sortBy((d, _) => d.ordinal)
      val minCostsSorted                                                = minCosts.filter(x => x._2 != Double.PositiveInfinity).toSeq.sortBy(e => e._2)
      for candidate <- minCostsSorted do
        // check for local assignment conflicts
        if localAssignments(candidate.dir.ordinal)._2 == (-1, -1) && !localAssignments.map(e => e._2)
            .contains(candidate.edge)
        then
          // Check for global assignment conflicts
          if assignments(candidate.edge._1)(candidate.dir.ordinal)._2 == (-1, -1)
            && assignments(candidate.edge._2)(candidate.dir.reverse.ordinal)._2 == (-1, -1)
          then
            // assigns an edge only if it does not cross any assigned edge
            if graph.edges.map(e =>
                !(isAligned(e, candidate.dir) && IntersectionTools().intersect(
                  e,
                  SimpleEdge(NodeIndex(candidate.edge._1), NodeIndex(candidate.edge._2)), init
                )),
              ).reduce(_ & _)
            then
              localAssignments(candidate.dir.ordinal) = (candidate.dir, candidate.edge)

              // add nodes to disjoint sets
              val sets = candidate.dir match
                case Direction.North | Direction.South => verticalSets
                case Direction.West | Direction.East   => horizontalSets
              val edge = candidate.edge
              val _    = sets.union(NodeIndex(edge._1), NodeIndex(edge._2))
              alingedEdges.+=(AlignedEdge(NodeIndex(candidate.edge._1), NodeIndex(candidate.edge._2), candidate.dir))
      end for
      // save assignments for node
      localAssignments.filter(e => e._2 != (-1, -1)).foreach(a => assignments(a._2._1)(a._1.ordinal) = a)
      // with reverse direction
      localAssignments.filter(e => e._2 != (-1, -1))
        .foreach(a => assignments(a._2._2)(a._1.reverse.ordinal) = (a._1, (a._2._2, a._2._1)))
    end for

    println(s"vertical Sets: ${verticalSets.values.foldLeft("")((s, e) => s.concat(e.toString()).toString())}")
    println(s"horizontal Sets: ${horizontalSets.values.foldLeft("")((s, e) => s.concat(e.toString()).toString())}")

    val alignedPos = pos.finish

    // create alignedGraph
    val alignedGraph = AlignedGraph.fromAlignedEdges(alingedEdges.toSeq).mkAlignedGraph

    alignedGraph
  end greedyAlignedGraph

end GreedyOrthogonalization
