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
import scala.collection.AbstractIterator
import scala.annotation.tailrec
import wueortho.util.GraphSearch.bfs
import wueortho.layout.FlowNetworkEdgeLength.getMapEdgeToAdjacentFace
import scala.collection.mutable.IndexedBuffer
import wueortho.util.GraphSearch
import wueortho.util.GraphSearch.dijkstra
import wueortho.util.GraphSearch.DijkstraCost
extension (a: Vec2D)
  def dot(b: Vec2D): Double =
    a.x1 * b.x1 + a.x2 * b.x2
  def det(b: Vec2D): Double = 
    a.x1 * b.x2 - a.x2 * b.x1

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

    //val allEdges2 = Seq((0, 1), (0, 3), (1, 2),(1, 11), (1, 9), (2, 3), (2, 4), (2, 6), (3, 4), (4, 5), (5, 6), (6, 7), (7,  8), (8, 10), (8, 9), (10, 11), (10, 12), (11, 12)).map(e => SimpleEdge(NodeIndex(e._1), NodeIndex(e._2)))
    //val allEdges2 = Seq((0, 1), (0, 3), (1, 11), (1, 9), (2, 3), (2, 4), (2, 6), (3, 4), (4, 5), (5, 6), (6, 7), (7,  8), (8, 10), (8, 9), (10, 11), (10, 12), (11, 13), (12, 13), (15, 16)).map(e => SimpleEdge(NodeIndex(e._1), NodeIndex(e._2)))
    //val posSeq2 = IndexedSeq(Vec2D(0, 0), Vec2D(0, 3), Vec2D(4, 3), Vec2D(4, 0), Vec2D(6, 2), Vec2D(7, 5), Vec2D(4, 6), Vec2D(2, 5), Vec2D(0, 6), Vec2D(-2, 5), Vec2D(0, 5), Vec2D(0, 4), Vec2D(1, 5), Vec2D(1, 4), Vec2D(1, 1), Vec2D(0.5, 2), Vec2D(3, 2))
    //val pos2 = VertexLayout(posSeq2)
    //val graph2 = Graph.fromEdges(allEdges2).mkBasicGraph

    //val c = allignAllUnalignedEdges(graph, aGraph,pos)
    //val d = IntersectionTools().intersect(allEdges2(0), allEdges2(1), pos2)
    //val e = traverseFace(graph2, pos2, SimpleEdge(NodeIndex(1), NodeIndex(2)), false).toSeq
    //val d = getAllRayIntersections(graph2, pos2, NodeIndex(14))
    //val f = getClosestIntersection(NodeIndex(14), pos2, d)
    //val g = testContainmentInFace(graph2, pos2, NodeIndex(14) )
    //val h = testOuterFace(graph2, SimpleEdge(NodeIndex(3), NodeIndex(0)), pos2)
    val allEdges3 = Seq(AlignedEdge(NodeIndex(0),NodeIndex(1), Direction.East), 
                        AlignedEdge(NodeIndex(0),NodeIndex(3), Direction.South),  
                        AlignedEdge(NodeIndex(1),NodeIndex(4), Direction.South), 
                        AlignedEdge(NodeIndex(4),NodeIndex(2), Direction.South), 
                        AlignedEdge(NodeIndex(2),NodeIndex(3), Direction.West),
                        AlignedEdge(NodeIndex(3),NodeIndex(5), Direction.South),
                        AlignedEdge(NodeIndex(5),NodeIndex(6), Direction.South),
                        AlignedEdge(NodeIndex(6),NodeIndex(7), Direction.East),
                        AlignedEdge(NodeIndex(2),NodeIndex(7), Direction.South),
                        AlignedEdge(NodeIndex(5),NodeIndex(8), Direction.East),
                        AlignedEdge(NodeIndex(2),NodeIndex(9), Direction.East),
                        AlignedEdge(NodeIndex(9),NodeIndex(10), Direction.South),
                        AlignedEdge(NodeIndex(7),NodeIndex(10), Direction.East),
                        AlignedEdge(NodeIndex(10),NodeIndex(11), Direction.South),
                        AlignedEdge(NodeIndex(17),NodeIndex(11), Direction.East),
                        AlignedEdge(NodeIndex(12),NodeIndex(13), Direction.North),
                        AlignedEdge(NodeIndex(3),NodeIndex(13), Direction.West),
                        AlignedEdge(NodeIndex(12),NodeIndex(14), Direction.East),
                        AlignedEdge(NodeIndex(14),NodeIndex(15), Direction.North),
                        AlignedEdge(NodeIndex(15),NodeIndex(16), Direction.East),
                        AlignedEdge(NodeIndex(17),NodeIndex(16), Direction.North),
                        AlignedEdge(NodeIndex(18),NodeIndex(20), Direction.North),
                        AlignedEdge(NodeIndex(18),NodeIndex(19), Direction.East),
                        )
    val vPosGraph3 = VertexLayout(IndexedSeq(
        Vec2D(4, -1),
        Vec2D(8, -1),
        Vec2D(8, -4),
        Vec2D(4, -4),
        Vec2D(8, -3),
        Vec2D(4, -6),
        Vec2D(4, -8),
        Vec2D(8, -8),
        Vec2D(6, -6),
        Vec2D(12, -4),
        Vec2D(12, -8),
        Vec2D(12, -12),//11
        Vec2D(1, -12),
        Vec2D(1, -4),
        Vec2D(5, -12),
        Vec2D(5, -10),
        Vec2D(8, -10),
        Vec2D(8, -12),
        Vec2D(5, -14), //18
        Vec2D(11, -14),
        Vec2D(6, -12),
      ))
    val graph3 = AlignedGraph.fromAlignedEdges(allEdges3).mkAlignedGraph
    val (_, dualG) = createDualGraph(graph3)
    //getPathDirString(graph3,  NodeIndex(5), NodeIndex(7), AlignedEdge(NodeIndex(3), NodeIndex(5), Direction.South), false)
    //println(dualG.vertices.zipWithIndex.map((v, i) => s"Knoten: ${i} hat Nachbarn: ${v.neighbors}\n"))
    //val fixSeq = fix180Turns(Seq(Direction.South,  Direction.East, Direction.South, Direction.North, Direction.East), true);
    //val fixSeqCW = fix180Turns(Seq(Direction.West, Direction.South, Direction.North, Direction.West,Direction.North), false);
    val unaligendEdges = Seq(SimpleEdge(NodeIndex(16), NodeIndex(18)))
    val graph3G = Graph.fromEdges(graph3.edges.map(_.unalign)++(unaligendEdges)).mkBasicGraph
    val newG = routeUnalignedEdge(graph3, graph3G, SimpleEdge(NodeIndex(12),NodeIndex(17)),vPosGraph3, graph3G.vertices.length)
    //val newG2 = connectTwoComponents(graph3, NodeIndex(12), NodeIndex(18), 22, vPosGraph3 )
    val newG4 = connectAllUnalignedComponents(graph3, graph3G, vPosGraph3)

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
    val iPos = ((pos(edge.to) - pos(edge.from)).scale(positionFactor)) + pos(edge.from)
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
    val rayEdge = SimpleEdge(start, NodeIndex(pos.nodes.size))
    var intersections: Seq[EdgeIntersectionWithPos] = Seq.empty
    def testBetween(e:SimpleEdge, test: NodeIndex)= 
      val kEdgeOrigin = (pos(e.to) - pos(e.from)).dot(posWithRay(test)-pos(e.from))
      val kEdgeEdge = (pos(e.to) - pos(e.from)).dot(pos(e.to) - pos(e.from))
      0 < kEdgeOrigin && kEdgeOrigin < kEdgeEdge
    for e <- graph.edges do
      if IntersectionTools().intersect(rayEdge, e, posWithRay, true) || (IntersectionTools().colinearityTest(rayEdge, e, posWithRay) && testBetween(rayEdge, e.from) )then 
        val point = if IntersectionTools().colinearityTest(rayEdge, e, posWithRay) then {
          // test between:
          if testBetween(e, rayEdge.from) then posWithRay(rayEdge.from)
          else if (pos(e.from) - pos(rayEdge.from)).len < (pos(e.to) - pos(rayEdge.from)).len then 
            pos(e.from) 
          else 
            pos(e.to)
        }
        else IntersectionTools().getIntersectionPoint(rayEdge, e, posWithRay)
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

  @tailrec
  def testContainmentInFace(graph: BasicGraph, pos: VertexLayout, start: NodeIndex, forbiddenEdges: Seq[SimpleEdge] = Seq.empty): Seq[SimpleEdge] = 
    def go(graph: BasicGraph, pos: VertexLayout, start: NodeIndex, forbiddenEdges: Seq[SimpleEdge]): (Boolean, Seq[SimpleEdge]) =
      val allIntersections = getAllRayIntersections(graph, pos, start, forbiddenEdges)
      if allIntersections.isEmpty then return (false, Seq.empty)
      val closestIntersection = getClosestIntersection(start, pos, allIntersections) 
      val (faceA, faceB) = (traverseFace(graph, pos, closestIntersection.get.e, true).toIndexedSeq, traverseFace(graph, pos, getReverseEdge(closestIntersection.get.e), true).toIndexedSeq) 
      //val (faceAIntersectedEdges, faceBIntersectedEdges) = (allIntersections.filter(i => faceA.contains(i.e) || faceA.contains(getReverseEdge(i.e))), allIntersections.filter(i => faceB.contains(i.e) || faceB.contains(getReverseEdge(i.e))))
      val (faceAIntersectedEdges, faceBIntersectedEdges) = (faceA.filter(e => allIntersections.exists(_.e == e) || allIntersections.exists(_.e == getReverseEdge(e))), faceB.filter(e => allIntersections.exists(_.e == e) || allIntersections.exists(_.e == getReverseEdge(e))))
      
      val posWithRay = VertexLayout(pos.nodes.appended(Vec2D(0, 1) + pos(start)))
      val rayEdge = SimpleEdge(start, NodeIndex(pos.nodes.size))
      
      //Do more precise counting/ special cases
      def getCriticalIntersections(faceIntersectedEdegs: Seq[SimpleEdge]):Int = 
        val criticalEdges = faceIntersectedEdegs.filter(e => IntersectionTools().throughNodeTest(rayEdge, e.from, posWithRay) || IntersectionTools().throughNodeTest(rayEdge, e.to, posWithRay))
        val prefix = criticalEdges.takeWhile(c => IntersectionTools().throughNodeTest(rayEdge, c.from, posWithRay))
        val criticalWrapped = criticalEdges.drop(prefix.size).appendedAll(prefix)
        // to start a critical sequence there must be a edge, where the start node is not colinear to the ray
        val criticalSequeces = criticalWrapped.foldLeft(Seq.empty[Seq[SimpleEdge]])((acc, x) => (acc, x) match
          case (Seq(), x) => Seq(Seq(x))
          case (acc, x) if !IntersectionTools().throughNodeTest(rayEdge, x.from, posWithRay) => acc :+ Seq(x) 
          case (Seq(_*), x) => acc.init:+(acc.last:+x)
        )
        val filteredSequences = criticalSequeces.filter(sequence => IntersectionTools().orientationTest(rayEdge, sequence.head.from, posWithRay) != IntersectionTools().orientationTest(rayEdge, sequence.last.to, posWithRay))
        val simpleIntersections = faceIntersectedEdegs.filter(e => !IntersectionTools().throughNodeTest(rayEdge, e.from, posWithRay) && !IntersectionTools().throughNodeTest(rayEdge, e.to, posWithRay))
        //val isolatedEdgeIntersections = simpleIntersections.map(e => SimpleEdge(NodeIndex(e.from.toInt min e.to.toInt),NodeIndex(e.from.toInt max e.to.toInt))).groupBy(identity).filter(_.size > 1)
        return filteredSequences.size + simpleIntersections.size // - isolatedEdgeIntersections.size
      end getCriticalIntersections
      
      val (intersectionsA, intersectionsB) = (getCriticalIntersections(faceAIntersectedEdges), getCriticalIntersections(faceBIntersectedEdges)) 

      if intersectionsA % 2 == 1 then return (true, faceA)
      if intersectionsB % 2 == 1 then return (true, faceB)
      (false, faceA.++(faceB))
    end go
    val face = go(graph, pos, start, forbiddenEdges)
    if face._2.isEmpty then return face._2
    if face._1 && !testOuterFace(graph, face._2.head, pos) then return face._2
    testContainmentInFace(graph, pos, start, forbiddenEdges.++(face._2))
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


  def testOuterFace(graph: BasicGraph, e: SimpleEdge, pos: VertexLayout): Boolean =
    val edges =  traverseFace(graph, pos, e, true).toSeq
    val cyclic = edges:++(edges.take(1))
    if cyclic.size < 3 then return false
    def angle(e1: SimpleEdge, e2: SimpleEdge): Double = 
      val vec1 = pos(e1.from)-pos(e1.to)
      val vec2 = pos(e2.to)-pos(e2.from)
      val dot = vec1.dot(vec2)
      val det = vec1.det(vec2)
      math.atan2(det, dot)
    end angle
    
    val angleSum = cyclic.sliding(2).map(l => angle(l.head, l.last)).sum
    angleSum < 0
  end testOuterFace

  def getConnectedComponent(graph: BasicGraph, startNode: NodeIndex): Set[NodeIndex] =
    val adj = (v: NodeIndex) => graph.vertices(v.toInt).neighbors.map(_.toNode)
    bfs.traverse(adj, startNode).toSet
  end getConnectedComponent

  def getConnectedComponents(graph: BasicGraph): Set[Set[NodeIndex]] =
    var uncheckedNodes                             = Range(0, graph.vertices.length).map(NodeIndex(_)).toSet
    val allComponents: mutable.Set[Set[NodeIndex]] = mutable.Set.empty
    while !uncheckedNodes.isEmpty do
      val startNode = uncheckedNodes.toSeq(0)
      val comp      = getConnectedComponent(graph, startNode)
      uncheckedNodes = uncheckedNodes.--(comp)
      allComponents.+=(comp)
    end while
    allComponents.toSet
  end getConnectedComponents

  def getOuterFace(graph: BasicGraph, pos: VertexLayout, componentEdges: Seq[SimpleEdge]): Seq[SimpleEdge] = 
    var remainingEdges = componentEdges.toSet
    while (!remainingEdges.isEmpty) do
      val currentEdge = remainingEdges.head
      if testOuterFace(graph, currentEdge, pos) then return traverseFace(graph,pos, currentEdge, true).toSeq
      val currentFace = traverseFace(graph,pos, currentEdge, true)
      remainingEdges = remainingEdges.--(currentFace)
    end while
    Seq.empty
  end getOuterFace

  def connectNestedComponents(graph: BasicGraph, pos: VertexLayout): BasicGraph = 
    val components = getConnectedComponents(graph)  
    var newEdges: Seq[SimpleEdge] = Seq.empty
    for comp <- components do
      val componentEdges = comp.flatMap(v => graph.vertices(v.toInt).neighbors.map(n => SimpleEdge(v,n.toNode)))
      val isContainedIn = testContainmentInFace(graph, pos, comp.head, componentEdges.toSeq)
      if !isContainedIn.isEmpty then
        val componentOuterFace = getOuterFace(graph, pos, isContainedIn)
        //TODO: Get outer face of component and connect to the face
        // TODO: get leftmost or bottommost node instead of first
         newEdges = newEdges:+(SimpleEdge(componentOuterFace.head.from, isContainedIn.head.from))
      end if
    end for
    return Graph.fromEdges(graph.edges:++(newEdges)).mkBasicGraph
    sys.error("No defined outer face of conected component!")
    graph
  end connectNestedComponents


  def angleOfEdge(e1: Vec2D, e2: Vec2D) =
    math.atan2((e2.x2 - e1.x2), (e2.x1 - e1.x1))

  def directionToAngle(dir: Direction) = dir match
    case Direction.East  => 0.0
    case Direction.North => Math.PI / 2
    case Direction.West  => Math.PI
    case Direction.South => -Math.PI / 2

  def edgeAlignmentCost(angle: Double, dir: Direction) =
    val semiAxisAngle = directionToAngle(dir)
    Math.abs(angle - semiAxisAngle) min Math.abs(angle - Math.PI * 2 - semiAxisAngle)


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


    // TODO: assumes, that graph is undirected weighted graph
    val undirectedGraph = sg2dg(graph)
    val allEdges = undirectedGraph.edges

    // precalculate all edge angles
    allEdges.map(b => (b.from, b.to, pos(b.from.toInt), pos(b.to.toInt)))
      .foreach((from, to, e1, e2) => allEdgeAngles(from.toInt, to.toInt) = angleOfEdge(e1, e2))

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
    var alignedGraph = AlignedGraph.fromAlignedEdges(alingedEdges.toSeq).mkAlignedGraph
    

    connectAllUnalignedComponents(alignedGraph, graph, init)

    // TODO: route all paths

    alignedGraph
  end greedyAlignedGraph

  def splitAlignedEdge(alignedGraph: AlignedGraph, edgeToSplit: AlignedEdge, newId: NodeIndex): AlignedGraph =
    val edgesWithoutSplitEdge = alignedGraph.edges.filter(e => !(e.from == edgeToSplit.from && e.to == edgeToSplit.to) && 
                                                !(e.to == edgeToSplit.from && e.from == edgeToSplit.to))
    val newEdges = edgesWithoutSplitEdge++Seq(AlignedEdge(edgeToSplit.from, newId, edgeToSplit.direction),
                                               AlignedEdge(newId, edgeToSplit.to, edgeToSplit.direction))
    AlignedGraph.fromAlignedEdges(newEdges).mkAlignedGraph
  end splitAlignedEdge 

  def getReverseEdge(e: AlignedEdge): AlignedEdge =
    AlignedEdge(e.to, e.from, e.direction.reverse)
  end getReverseEdge

  /**
    * routes one unaligned Edge as a path throught the graph
    *
    * @param alignedGraph
    * @param graph
    * @param unaligendEdge
    * @param pos
    * @param newNodeId
    * @return
    */
  def routeUnalignedEdge(alignedGraph: AlignedGraph, graph: BasicGraph, unaligendEdge: SimpleEdge, pos: VertexLayout, newNodeId: Int): AlignedGraph =
    // wenn die Kante gesplittet wird, wird einen neuen start-/Endknoten verwendet
    var startNode = unaligendEdge.from
    var endNode = unaligendEdge.to
    var newId = newNodeId;
    
    var newAlignedGraph = alignedGraph
    val (newG, startFaceEdge, newId1) = findRadialNextAlignedEdgeOrSplitEdge(newAlignedGraph, unaligendEdge, pos, newId)
    if newG.vertices.length > newAlignedGraph.vertices.length then startNode = NodeIndex(newG.vertices.length - 1)
    newAlignedGraph = newG
    newId = newId1
    val (newG2, endFaceEdge, newId2) = findRadialNextAlignedEdgeOrSplitEdge(newAlignedGraph, getReverseEdge(unaligendEdge), pos, newId) 
      //findRadialNextAlignedEdge(alignedGraph, graph, getReverseEdge(unaligendEdge), pos)
    if newG2.vertices.length > newAlignedGraph.vertices.length then endNode = NodeIndex(newG2.vertices.length - 1)
    newAlignedGraph = newG2
    newId = newId2

    // bestimmte dualGraph 
    val (faceReps, dualG) = createDualGraph(newAlignedGraph)


    var startFace = newAlignedGraph.traverseEdgesAlignedFace(startFaceEdge, false, false).toSeq.filter(faceReps.contains(_)).head
    val startFaceIndex = faceReps.indexOf(startFace)
    var endFace = newAlignedGraph.traverseEdgesAlignedFace(endFaceEdge, false, false).toSeq.filter(faceReps.contains(_)).head
    val endFaceIndex = faceReps.indexOf(endFace)



    // TODO: finde kürzesten Weg durch dualgraph
    // TODO: Es sollte für alle möglichen Facetten, die an dem Knoten angrenzend sind geschaut
    //       werden, welche zu dem kürzesten Pfad führt und diese dann wählen
    given DijkstraCost[Int, Int] = (a, b) => a + b
    val dij = dijkstra[Int, Int]
    val shortestPath = dij.shortestPath((x:NodeIndex) => dualG.vertices(x.toInt).neighbors.map(l => (l.toNode, 1)) , NodeIndex(startFaceIndex), NodeIndex(endFaceIndex), 0) 
    val facesInPath = shortestPath match
      case Left(value) => sys.error("Error while calculating dijkstra on dual graph!") 
      case Right(path) => path.nodes
    
    var edgesToSpit:Seq[AlignedEdge] = Seq()
    if facesInPath.size > 1 then
      // TODO: finde alle Kanten zwischen den Facetten, auf dem kürzesten Weg
      edgesToSpit = facesInPath.sliding(2).map(l => getEdgeBetweenFaces(newAlignedGraph,l(0).toInt, l(1).toInt)).flatMap(x => x match
        case Some(value) => Seq(value)
        case None => Seq()
      ).toSeq
      
    // splitte diese Kanten zwischen den Facetten
    var newPathNodesWithFace = Seq(startNode)
    for edge <- edgesToSpit do
      newAlignedGraph = splitAlignedEdge(newAlignedGraph, edge, NodeIndex(newId))
      // update start- or endEdge, if it was just split
      if edge == startFace then 
        startFace = newAlignedGraph.edges.filter(e => e.from == startFace.from && e.to == NodeIndex(newId)).head
      if getReverseEdge(edge) == startFace then
        startFace = newAlignedGraph.edges.map(getReverseEdge(_)).filter(e => e.from == startFace.from && e.to == NodeIndex(newId)).head
      if edge == endFace then 
        endFace = newAlignedGraph.edges.filter(e => e.from == endFace.from && e.to == NodeIndex(newId)).head
      if getReverseEdge(edge) == endFace then
        endFace = newAlignedGraph.edges.map(getReverseEdge(_)).filter(e => e.from == endFace.from && e.to == NodeIndex(newId)).head
      
      newPathNodesWithFace = newPathNodesWithFace.appended(NodeIndex(newId))
      newId = newId + 1
    end for
    newPathNodesWithFace = newPathNodesWithFace.appended(endNode) 

    // Es muss durch die Knoten, die geschnitten werden iteriert werden und durch die facetten !!!
    // verwende getPathDirString pro Facette, um die Knicke zu bestimmen
    var allNewEdges: Seq[AlignedEdge] = Seq()
    for (nodes, faceId) <- newPathNodesWithFace.sliding(2).zip(facesInPath) do
      val startNode = nodes.head
      val endNode = nodes.last // should be element 1
      if startNode == endNode then sys.error("start and endnode are equal, can not route between those nodes!")
      val faceRep = faceReps(faceId.toInt)
      val pathString = getPathDirString(newAlignedGraph,startNode, endNode, faceRep, false).map(charToDir(_))
      // creates a path from Directions
      val newEdges = dirSeqToAlignedEdgeSeq(pathString, startNode, endNode, newId)
      newId = newId + (newEdges.length - 1 max 0) 
      allNewEdges = allNewEdges++(newEdges)
    end for

    
    //Füge diese Kanten zum Graph hinzu??
    AlignedGraph.fromAlignedEdges(newAlignedGraph.edges++(allNewEdges)).mkAlignedGraph

  end routeUnalignedEdge

  /**
    * creates a sequence of alignedEdge starting at startNode, ending at endNode with given directions 
    * and using ids starting with newNodeId for new nodes 
    *
    * @param dirSeq
    * @param startNode
    * @param endNode
    * @param newNodeId
    * @return
    */
  def dirSeqToAlignedEdgeSeq(dirSeq: Seq[Direction], startNode: NodeIndex, endNode: NodeIndex, newNodeId: Int): Seq[AlignedEdge] = 
    val nodeIds = Seq(startNode)++(Range(newNodeId, dirSeq.length).map(NodeIndex(_))).appended(endNode)
    nodeIds.sliding(2).zip(dirSeq).map((l, dir) => AlignedEdge(l(0), l(1), dir)).toSeq
  end dirSeqToAlignedEdgeSeq


  def getOutgoingEdgeInSameFace(alignedGraph: AlignedGraph, node: NodeIndex, dir: Direction): AlignedEdge = 
    var curDir = dir
    if !alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).isEmpty then return alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).map(l => AlignedEdge(node, l.toNode, l.direction)).head
    curDir = curDir.turnCCW
    if !alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).isEmpty then return alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).map(l => AlignedEdge(node, l.toNode, l.direction)).head
    curDir = curDir.turnCCW
    if !alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).isEmpty then return alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).map(l => AlignedEdge(node, l.toNode, l.direction)).head
    curDir = curDir.turnCCW
    if !alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).isEmpty then return alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == curDir.turnCCW).map(l => AlignedEdge(node, l.toNode, l.direction)).head
    curDir = curDir.turnCCW
    sys.error("the node has no aligned edges, thus is not connected")
    alignedGraph.vertices(node.toInt).neighbors.map(l => AlignedEdge(node, l.toNode, l.direction)).head
  end getOutgoingEdgeInSameFace

  /**
    * Given a unaligned edge it finds the next aligned edge in ordinal ordering to find the face, the edge is inside
    *
    * @param alignedGraph
    * @param graph
    * @param unaligendEdge
    * @param pos
    * @return
    */
  def findRadialNextAlignedEdgeOrSplitEdge(alignedGraph: AlignedGraph, unaligendEdge: SimpleEdge, pos: VertexLayout, newNodeId: Int): (AlignedGraph, AlignedEdge, Int) =
    val usedDir = alignedGraph.vertices(unaligendEdge.from.toInt).neighbors.map(_.direction)
    val freeDir = Set(Direction.North, Direction.East, Direction.South, Direction.West)--(usedDir).toSeq
    if !freeDir.isEmpty then
      val selectedDir = freeDir.head // the direction, the unaligned face will go to
      val nextAligendEdge = getOutgoingEdgeInSameFace(alignedGraph, unaligendEdge.from, selectedDir)
      (alignedGraph, nextAligendEdge, newNodeId)
    else 
      // TODO split edge
      val edgeToSplit = alignedGraph.vertices(unaligendEdge.from.toInt).neighbors.map(l => AlignedEdge(unaligendEdge.from, l.toNode, l.direction)).head
      val newGraph = splitAlignedEdge(alignedGraph, edgeToSplit, NodeIndex(newNodeId))
      (newGraph, AlignedEdge(edgeToSplit.from, NodeIndex(newNodeId), edgeToSplit.direction), newNodeId + 1)
  end findRadialNextAlignedEdgeOrSplitEdge

  /**
    * fixes 180 turns for a given direction sequence
    *
    * @param edges
    * @param cw
    * @return
    */
  def fix180Turns(edges: Seq[Direction], cw: Boolean): Seq[Direction] = 
    edges.appended(edges.last).sliding(2, 1).flatMap(l => 
        if l(1) == l(0).reverse then 
           Seq(l(0), 
            cw match
              case true => l(0).turnCW
              case false => l(0).turnCCW
            )
        else Seq(l(0))
      ).toSeq
  end fix180Turns


  def dirToChar(dir: Direction): String = dir match
      case Direction.North => "N"
      case Direction.East => "E"
      case Direction.South => "S"
      case Direction.West => "W"

  def charToDir(str: Char): Direction = str match
      case 'N' => Direction.North
      case 'E' => Direction.East
      case 'S' => Direction.South
      case 'W' => Direction.West


  /**
    * returns the shortest path (directions as string literals) through a face with a given starting point and endpoint
    *
    * @param alignedGraph
    * @param startNode
    * @param endNode
    * @param faceRep
    * @param cw
    * @return
    */
  def getPathDirString(alignedGraph: AlignedGraph, startNode: NodeIndex, endNode: NodeIndex, faceRep: AlignedEdge, cw: Boolean): String = 
    //get first edge adjacent to startNode
    val startingEdges = alignedGraph.traverseEdgesAlignedFace(faceRep, false, false).toSeq.dropWhile(l => l.from != startNode)
    if startingEdges.isEmpty then sys.error("starting Edge was not found when traversing face!")
    val startEdge = startingEdges.head
    val faceEdges = Seq(startEdge)++alignedGraph.traverseEdgesAlignedFace(startEdge, false,false).toSeq
    val pathCW = faceEdges.takeWhile(edge => edge.from != endNode)
    val pathCCWReversed = faceEdges.dropWhile(edge => edge.from != endNode).takeWhile(edge => edge.from != startNode)
    
    //turn to directions
    val dirCW = fix180Turns(pathCW.map(_.direction), true)
    val dirCCW = fix180Turns(pathCCWReversed.map(_.direction), true).reverse.map(_.reverse)

    // add extra edge to connect to existing node
    val dirCWWithPrefix = Seq(dirCW.head.turnCW)++dirCW++Seq(dirCW.last.turnCCW)
    val dirCCWWithPrefix = Seq(dirCCW.head.turnCCW)++dirCCW++Seq(dirCCW.last.turnCW)

    //turn to string
    var dirCWString: String = dirCWWithPrefix.map(dirToChar(_)).reduce(_+_)
    var dirCCWString: String = dirCCWWithPrefix.map(dirToChar(_)).reduce(_+_)

    //apply all simplfication rules
    val replacements = Map(
      "NWN" -> "NN",
      "NEN" -> "NN",
      "ENE" -> "EE",
      "ESE" -> "EE",
      "SWS" -> "SS",
      "SES" -> "SS",
      "WNW" -> "WW",
      "WSW" -> "WW",
      "NN" -> "N",
      "EE" -> "E",
      "SS" -> "S",
      "WW" -> "W",
    )

    val pattern = replacements.keys
      .map(java.util.regex.Pattern.quote)
      .mkString("|")
      .r

    while pattern.findFirstIn(dirCWString).isDefined do
      dirCWString = pattern.replaceAllIn(dirCWString, m => replacements(m.matched))
    end while

    while pattern.findFirstIn(dirCCWString).isDefined do
      dirCCWString = pattern.replaceAllIn(dirCCWString, m => replacements(m.matched))
    end while

    return if dirCWString.length() < dirCCWString.length() then dirCWString else dirCCWString
  end getPathDirString


  /**
    * returns some of an aligned edge, if both faces share an edge, none otherwiese
    *
    * @param graph
    * @param faceA
    * @param faceB
    * @return
    */
  def getEdgeBetweenFaces(graph: AlignedGraph, faceA: Int, faceB: Int): Option[AlignedEdge] =
    val faceReps = graph.getOneEdgePerFace();

    // get map from edge to faces
    val edgeToFaceMap = getMapEdgeToAdjacentFace(graph, faceReps)

    val sharedEdge = edgeToFaceMap.filter(entry => entry._2.contains(faceA) && entry._2.contains(faceB))

    if sharedEdge.isEmpty then None
    else Some(sharedEdge.head._1)
    

  end getEdgeBetweenFaces


  /**
    * Creates a dual graph of a connected graph
    *
    * @param graph
    * @return
    */
  def createDualGraph(graph: AlignedGraph): (IndexedBuffer[AlignedEdge], BasicGraph) = 
    //TODO: Eventuell muss hier auch zusätzlich die faceReps zurückgegeben werden
    val faceReps = graph.getOneEdgePerFace();

    // get map from edge to faces
    val edgeToFaceMap = getMapEdgeToAdjacentFace(graph, faceReps)

    val dualGraphEdges = edgeToFaceMap.flatMap((_, adjFacesIndices) =>
      if adjFacesIndices.size == 0 then Seq()
      else if adjFacesIndices.size == 1 then Seq()//no self edges //Seq(SimpleEdge(NodeIndex(adjFacesIndices.head), NodeIndex(adjFacesIndices.head)))
      else Seq(SimpleEdge(NodeIndex(adjFacesIndices.head), NodeIndex(adjFacesIndices.tail.head)))
    ).toSeq
      .distinct
      .filter(e => e.to != e.from)

    (faceReps, Graph.fromEdges(dualGraphEdges).mkBasicGraph)
  end createDualGraph


  def connectAllUnalignedComponents(alignedGraph: AlignedGraph, graph: BasicGraph, pos: VertexLayout): AlignedGraph =
    // connect connected components, that are connected by unaligned edges
    val gConnenctedComps = getConnectedComponents(graph).toSeq
    var newAlignedGraph = alignedGraph
    var newNodeId: Int = graph.vertices.length
    
    // TODO: Only consider connected components in G
    // TODO: iterate over the connected components in G and only look at connected components 
    // induced by those nodes
    
    for conComp <- gConnenctedComps do
      var inducedGAlignedComps = getConnectedComponents(
        Graph.fromEdges(newAlignedGraph.edges.filter(e => conComp.contains(e.from) || conComp.contains(e.to)).map(_.unalign)).mkBasicGraph
        ).toSeq

      var filteredComps = inducedGAlignedComps.filter(l => l.forall(conComp.contains(_)))

    
      // TODO: Needs to be rewritten
      // TODO: This is wrong
      while filteredComps.size > 1 do
        var curMinComp = (0, 1)
        var curMin = (filteredComps(0).toSeq.head, filteredComps(1).toSeq.head)
        // TODO: bestimme das Paar von connected Components, das am nächsten ist
        for i <- Range(0, filteredComps.size) do
          for j <- Range(i + 1, filteredComps.size) do
            // TODO: get min and compare
            for u <- filteredComps(i) do 
              for v <- filteredComps(j) do
                if (pos(u)-pos(v)).len < (pos(curMin._1)-pos(curMin._2)).len then
                  curMin = (u, v)
                  curMinComp = (i, j)
                end if
              end for
            end for 
          end for
        end for 

        // Verbinde die beiden connected components
        val (newAlignedGraph3, newId2) = connectTwoComponents(newAlignedGraph, curMin._1, curMin._2, newNodeId, pos)
        newAlignedGraph = newAlignedGraph3
        newNodeId = newId2

        // update while condiction
        inducedGAlignedComps = getConnectedComponents(
          Graph.fromEdges(newAlignedGraph.edges.filter(e => conComp.contains(e.from) || conComp.contains(e.to)).map(_.unalign)).mkBasicGraph
          ).toSeq

        filteredComps = inducedGAlignedComps.filter(l => l.forall(conComp.contains(_)))


      end while // more than on connected (aligned) component

    end for

    newAlignedGraph
  end connectAllUnalignedComponents

  def connectTwoComponents(alignedGraph: AlignedGraph, nodeA: NodeIndex, nodeB: NodeIndex,  newNodeId: Int, pos: VertexLayout):(AlignedGraph, Int) = 
    def hasNoEdgeInDir(node: NodeIndex, dir: Direction): Boolean =
      alignedGraph.vertices(node.toInt).neighbors.filter(_.direction == dir).isEmpty
    
    var newAlignedGraph = alignedGraph
    var newEdges: Seq[AlignedEdge] = Seq()
    var newId = newNodeId
    var newNodeA = nodeA
    var newNodeB = nodeB

    // NODE A
    // finde die richting (semiachse), zu der die Kante zwischen den Knoten am nächsten sind
    val dirAtoB = Seq(Direction.North, Direction.West, Direction.South, Direction.East)
      .sortBy(dir => edgeAlignmentCost(angleOfEdge(pos(nodeA), pos(nodeB)), dir)).head
    if !hasNoEdgeInDir(nodeA, dirAtoB) then  
      // alternativ orthogonal angrenzenden Richtung mit Knick oder Kante splitten
      val dirAtoBSecond = Seq(Direction.North, Direction.West, Direction.South, Direction.East)
      .sortBy(dir => edgeAlignmentCost(angleOfEdge(pos(nodeA), pos(nodeB)), dir)).tail.head
      val edgeToSplit = alignedGraph.vertices(nodeA.toInt).neighbors.filter(_.direction == dirAtoBSecond) 
      if hasNoEdgeInDir(nodeA, dirAtoBSecond) then 
        newEdges = newEdges.appended(AlignedEdge(nodeA, NodeIndex(newId), dirAtoBSecond))
        newNodeA = NodeIndex(newId)
        newId = newId + 1
      else
        newAlignedGraph = splitAlignedEdge(newAlignedGraph,  edgeToSplit.map(l => AlignedEdge(nodeA, l.toNode, l.direction)).head, NodeIndex(newId)) 
        newNodeA = NodeIndex(newId)
        newId = newId + 1

    // NODE 
    // finde die richting (semiachse), zu der die Kante zwischen den Knoten am nächsten sind
    val dirBtoA = Seq(Direction.North, Direction.West, Direction.South, Direction.East)
      .sortBy(dir => edgeAlignmentCost(angleOfEdge(pos(nodeB), pos(nodeA)), dir)).head
    if !hasNoEdgeInDir(nodeB, dirBtoA) then
      // alternativ orthogonal angrenzenden Richtung mit Knick oder Kante splitten
      val dirBtoASecond = Seq(Direction.North, Direction.West, Direction.South, Direction.East)
        .sortBy(dir => edgeAlignmentCost(angleOfEdge(pos(nodeB), pos(nodeA)), dir)).tail.head
      val edgeToSplit = alignedGraph.vertices(nodeB.toInt).neighbors.filter(_.direction == dirBtoASecond) 
      if hasNoEdgeInDir(nodeB, dirBtoASecond) then 
        newEdges = newEdges.appended(AlignedEdge(nodeA, NodeIndex(newId), dirBtoASecond))
        newNodeB = NodeIndex(newId)
        newId = newId + 1
      else
        newAlignedGraph = splitAlignedEdge(newAlignedGraph,  edgeToSplit.map(l => AlignedEdge(nodeB, l.toNode, l.direction)).head, NodeIndex(newId)) 
        newNodeB = NodeIndex(newId)
        newId = newId + 1


    //add connecting edge
    newEdges = newEdges.appended(AlignedEdge(newNodeA, newNodeB, dirAtoB))

    (AlignedGraph.fromAlignedEdges(newAlignedGraph.edges.++(newEdges)).mkAlignedGraph, newId) 
  end connectTwoComponents

end GreedyOrthogonalization

//case class Face(faceRep: AlignedEdge, edges: IndexedSeq[AlignedEdge])
