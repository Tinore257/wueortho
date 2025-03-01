// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout, WeightedGraph}
import wueortho.data.Direction
import scala.collection.mutable
import wueortho.data.WeightedEdge
import wueortho.util.mutable.DisjointSets
import wueortho.util.Monoid
import wueortho.data.mutable.Matrix.fill
import wueortho.data.NodeIndex
import wueortho.util.GraphConversions.wg2wd
import wueortho.data.DiGraph
import scala.compiletime.ops.double
import wueortho.data.Graph
import wueortho.data.SimpleEdge
import wueortho.util.GraphSearch.bfs
import wueortho.data.VertexBoxes
import wueortho.data.Rect2D
import wueortho.data.WeightedDiGraph


object GreedyOrthogonalization:
  
  def topologicalSort(ids: Seq[NodeIndex],neighbors: NodeIndex => Seq[NodeIndex]):Seq[NodeIndex] = 
    val visited = mutable.BitSet.empty
    val finished = mutable.BitSet.empty
    val result  = mutable.ArrayBuffer.empty[NodeIndex]

    var cycleFlag = false

    def visit(v: NodeIndex):Unit =
       if !cycleFlag && !finished.contains(v.toInt) then
        if visited.contains(v.toInt) then cycleFlag = true
        val _ = visited.add(v.toInt)
        for u <- neighbors(v) do
          visit(u)
        val _ = finished.add(v.toInt)
        result.append(v)

    //get nodes without ingoing edges
    val verticesWithIngoingEdges =  ids.flatMap(id => neighbors(id))

    for v <- ids do
      if !verticesWithIngoingEdges.contains(v) then visit(v)
      if cycleFlag then println("no topological ordering because of cycles!")

    return result.toSeq
  end topologicalSort


  def layout(graph: WeightedGraph, init: VertexLayout, boxes: VertexBoxes): VertexLayout =
    val n = graph.numberOfVertices
    
    class PosVec(init: Seq[Vec2D]):
      var a = Array[Vec2D](init*)
      
      def apply(i: Int) = a(i)
      def finish        = a.toVector
    
      def update(i: Int, value: Vec2D) = a(i) = value
      
    end PosVec
      
    val pos = PosVec(init.nodes);

    if n < 2 then return VertexLayout(pos.finish)
        
    given Monoid[Set[Int]] with
      def zero: Set[Int] = Set.empty
      override def apply(a: Set[Int], b: Set[Int]): Set[Int] = a union b

    val verticalSets = DisjointSets[Int, Set[Int]]
    val horizontalSets = DisjointSets[Int, Set[Int]]

    val assignments: IndexedSeq[mutable.IndexedSeq[(Direction,(Int, Int))]] = IndexedSeq(init.nodes.map(_ => Direction.values.map(d => (d, (-1,-1))).sortBy((d, _) => d.ordinal))*)

    val allEdgeAngles = wueortho.data.mutable.Matrix.fill(n, n)(0)

    def angleOfEdge(e1: Vec2D, e2: Vec2D) = 
       math.atan2((e2.x2-e1.x2),(e2.x1-e1.x1))
    
    //TODO: assumes, that graph is undirected weighted graph
    val undirectedGraph = wg2wd(graph)

    val allEdges = undirectedGraph.edges

    //precalculate all edge angles
    allEdges.map(b => (b.from, b.to, pos(b.from.toInt), pos(b.to.toInt)))
      .foreach((from, to, e1, e2) => allEdgeAngles(from.toInt, to.toInt) = angleOfEdge(e1, e2))

    def directionToAngle(dir: Direction) = dir match
        case Direction.East => 0.0
        case Direction.North => Math.PI/2
        case Direction.West => Math.PI
        case Direction.South => -Math.PI/2
    

    def edgeAlignmentCost(angle: Double, dir: Direction) = 
      val semiAxisAngle = directionToAngle(dir)
      Math.abs(angle - semiAxisAngle) min Math.abs(angle - Math.PI*2 - semiAxisAngle)
    
    def intersect(e1: WeightedEdge, e2: WeightedEdge):Boolean =
      def orientationTest(e:WeightedEdge, node:NodeIndex):Double = 
        val p = pos(e.from.toInt)
        val q = pos(e.to.toInt)
        val r = pos(node.toInt)
        (q.x2 - p.x2) * (r.x1 - q.x1) -
        (q.x1 - p.x1) * (r.x2 - q.x2)

      def onSegment(p1: NodeIndex, p2: NodeIndex, node: NodeIndex) = 
        val p = pos(p1.toInt)
        val q = pos(p2.toInt)
        val r = pos(node.toInt)
        q.x1 <= (p.x1 max r.x1) && q.x1 >= (p.x1 min r.x1) &&
        q.x2 <= (p.x2 max r.x2) && q.x2 >= (p.x2 min r.x2)
      
      val o1 = orientationTest(e1, e2.from)
      val o2 = orientationTest(e1, e2.to)
      val o3 = orientationTest(e2, e1.from)
      val o4 = orientationTest(e2, e1.to)

      //general case
      if (o1 != o2 && o3 != o4) then return true
      
      //edge-cases with colinearity
      if (o1 == 0 && onSegment(e1.from, e2.from, e1.to)) then return true
      if (o2 == 0 && onSegment(e1.from, e2.to, e1.from)) then return true
      if (o3 == 0 && onSegment(e2.from, e1.from, e2.to)) then return true
      if (o4 == 0 && onSegment(e2.from, e1.to, e2.to)) then return true
      false
 

    val verticesOrderedByDegree = undirectedGraph.vertices
      .zipWithIndex
      .map((v, i) => (i, v.neighbors.length))
      .sortBy((_,l) => l)
      .reverse

    def isAligned(e: WeightedEdge) = 
      verticalSets.contains(e.from.toInt) && verticalSets.contains(e.to.toInt) 
      || horizontalSets.contains(e.from.toInt) && horizontalSets.contains(e.to.toInt) 

    case class Candidate(dir: Direction, weight: Double, edge:(Int, Int))

    for (vertex, deg) <- verticesOrderedByDegree do
      //calculate cost function for all edges starting at v
      val minCosts: mutable.IndexedSeq[Candidate] = Direction.values.map(d => Candidate(d, Double.PositiveInfinity, (0,0))).sortBy(_.dir.ordinal)

      //get for each direction edge within 45 degrees with minimal costs
      for neighbor <- undirectedGraph.vertices(vertex).neighbors.map(v => v.toNode.toInt) do
        for dir <- Direction.values do
          val cost = edgeAlignmentCost(allEdgeAngles(vertex, neighbor), dir)
          if minCosts(dir.ordinal)._2 > cost && math.abs(cost) < Math.PI/4
            then minCosts(dir.ordinal) = Candidate(dir, cost, (vertex, neighbor))
      
      //check for conflicts (i.e. double assignments, overrides) and maintain disjoint sets
      val localAssignments: mutable.IndexedSeq[(Direction, (Int, Int))] = Direction.values.map(d => (d, (-1,-1))).sortBy((d, _) => d.ordinal)
      val minCostsSorted = minCosts.filter(x => x._2 != Double.PositiveInfinity).toSeq.sortBy(e => e._2)
      for candidate <- minCostsSorted do 
        //check for local assignment conflicts
        if localAssignments(candidate.dir.ordinal)._2 == (-1,-1) && !localAssignments.map(e => e._2).contains(candidate.edge) then
          //Check for global assignment conflicts
          if assignments(candidate.edge._1)(candidate.dir.ordinal)._2 == (-1,-1) && assignments(candidate.edge._2)(candidate.dir.reverse.ordinal)._2 == (-1,-1) then
            //assigns an edge only if it does not cross any assigned edge
            if graph.edges.foldLeft(true)((acc, e) => acc &&  (!isAligned(e) || !intersect(e, WeightedEdge(NodeIndex(candidate.edge._1),NodeIndex(candidate.edge._2), 1.0)))) then 
              localAssignments(candidate.dir.ordinal) = (candidate.dir, candidate.edge)

              //add nodes to disjoint sets
              val sets = candidate.dir match
                case Direction.North | Direction.South => verticalSets
                case Direction.West | Direction.East => horizontalSets
              val edge = candidate.edge
              if !sets.contains(edge._1) then
                val _ = sets.mkSet(edge._1,  Set(edge._1))
              if !sets.contains(edge._2) then
                val _ = sets.mkSet(edge._2,  Set(edge._2))
              val _ = sets.union(edge._1, edge._2)
        
      //save assignments for node
      localAssignments.filter(e => e._2 != (-1, -1)).foreach(a => assignments(a._2._1)(a._1.ordinal) = a)
      //with reverse direction
      localAssignments.filter(e => e._2 != (-1, -1)).foreach(a => assignments(a._2._2)(a._1.reverse.ordinal) = (a._1, (a._2._2, a._2._1)))

    //calculate (median) positions for each disjoint set and set nodes position to median
    for vertices <- verticalSets.values if vertices.size > 0 do
      val medianSeq = vertices.toSeq.map(v => pos(v).x1).sorted
      val median = medianSeq(Math.floor(vertices.size.toDouble/2.0).toInt)
      vertices.foreach(v => pos.update(v,Vec2D(median, pos(v).x2)))

    for v <- horizontalSets.values if v.size > 0 do
      val median = v.toSeq.map(v => pos(v).x2).sorted()(Math.floor(v.size/2.0).toInt)
      v.foreach(v => pos.update(v, Vec2D(pos(v).x1, median)))

    println(s"vertical Sets: ${verticalSets.values.foldLeft("")((s,e) => s.concat(e.toString()).toString() )}")
    println(s"horizontal Sets: ${horizontalSets.values.foldLeft("")((s,e) => s.concat(e.toString()).toString() )}")      


        

    //TODO: topological ordering
    def compactGraph(graph: WeightedDiGraph, dir:Direction, disjointSets: DisjointSets[Int, Set[Int]]) = 

      //TODO: Change to Either[NodeIndex, Int]
      def createMappingContracted2Normal(sets: DisjointSets[Int, Set[Int]]): IndexedSeq[Either[Int, Int]] = 
        //TODO: Construct graph with merged vertical nodes (and removed multi-edges)

        //contains only the mapping for nodes that are contained in a set
        var nodeId2SetMap:mutable.IndexedSeq[Int] = mutable.IndexedSeq(graph.vertices.map(_ => -1)*)
        sets.values.zipWithIndex
          .foreach((vertexSet, setId) => vertexSet.foreach(v => nodeId2SetMap(v) = setId))
        
        val numberContractedVertices =  nodeId2SetMap.filter(_ == -1).size + sets.values.size
        
        val uncontractedVertices = nodeId2SetMap.zipWithIndex.filter(_._1 == -1).map((_, vertexId) => Left(vertexId))
        val contractedVertices = sets.values.zipWithIndex.map((_, setId) => Right(setId))
        //map contracted =>  Either[vertexId, setId]
        val contracted2Normal = uncontractedVertices.appendedAll(contractedVertices)

        return contracted2Normal.toIndexedSeq

      def createMappingNormal2Contracted(contracted2Normal: IndexedSeq[Either[Int, Int]], sets: DisjointSets[Int, Set[Int]]): IndexedSeq[Int] = 
        //TODO: Check if this will always work!
        val normal2ContractedUnchecked = contracted2Normal.zipWithIndex
          .flatMap((e, contrId) => e match
            case Left(vertexId) => Seq((vertexId, contrId))
            case Right(setId) => sets.values(setId).map(vertexId => (vertexId,contrId))
          ).sortBy((vertexId,_) => vertexId)
          
        val check = normal2ContractedUnchecked.foldLeft(Option(-1))((acc, x) => acc match
          case Some(a) => if x._1 == a + 1 then Some(x._1) else None
          case None => None
        )

        check match {
          case None => println("Vertex indices were not ascending")
          case _ => //vertices ordered ascending without skips
        }
        
        return normal2ContractedUnchecked.map((_, a) => a)

      def createContractedGraph(graph: WeightedDiGraph,numberOfVertices: Int, mappingNormal2Contracted: IndexedSeq[Int], dir: Direction):DiGraph = 

        def testEdgeFacing(e: WeightedEdge, dir: Direction) = dir match
          case Direction.East => (pos(e.from.toInt).x1 < pos(e.to.toInt).x1)
          case Direction.West => (pos(e.from.toInt).x1 > pos(e.to.toInt).x1)
          case Direction.North => (pos(e.from.toInt).x2 < pos(e.to.toInt).x2)
          case Direction.South => (pos(e.from.toInt).x2 > pos(e.to.toInt).x2) 

        def testSelfEdge(e: WeightedEdge) = 
          e.from == e.to

        val allEdgesWithCorrectDirection = graph.edges.filter(e => testEdgeFacing(e, dir) && !testSelfEdge(e))

        val edgesInContractedGraph = allEdgesWithCorrectDirection.map(e => (mappingNormal2Contracted(e.from.toInt), mappingNormal2Contracted(e.to.toInt)))
          .distinct
          .map(e => SimpleEdge(NodeIndex(e._1), NodeIndex(e._2)))
          .filter(e => !testSelfEdge(e.withWeight(1.0)))

        return Graph.fromEdges(edgesInContractedGraph, numberOfVertices).mkDiGraph

      val mappingContracted2Normal = createMappingContracted2Normal(disjointSets)
      val numberOfContractedVertices = mappingContracted2Normal.size
      val mappingNormal2Contracted = createMappingNormal2Contracted(mappingContracted2Normal, disjointSets)

      //graph with vertial aligned vertices contracted, TODO: does this need to be in the oppositve order
      val contracedDiGraph = createContractedGraph(graph, numberOfContractedVertices, mappingNormal2Contracted, dir)  
      
      println(s"All Edges facing ${dir.toString()}")
      println(s"contracted graph ${contracedDiGraph.vertices.zipWithIndex.map((e,i) => s"${i} -> ${e.toString()} \n" ).toString()}")

      val topSort = topologicalSort(contracedDiGraph.vertices.zipWithIndex.map((_, i) => NodeIndex(i)), x => contracedDiGraph.vertices(x.toInt).neighbors)
      
      println(topSort)

      if topSort.length > 0 then

        //TODO Only align, if there are more than two nodes in topological ordering
        //allignW every node v such that it has minimum position of all nodes {u1, ..., uk} with (v, u_i) in directed Edges - size of v
        //does not change position if there is no outgoing edge
        //val rightAlignedContractedPos = topSort.foldLeft(IndexedSeq((topSort.last, pos(topSort.last.toInt))):IndexedSeq[(NodeIndex, Vec2D)])((acc, v) => )
        var contractedPos = mutable.IndexedSeq(mappingContracted2Normal.map(normal => normal match
          case Left(i) => pos(i)
          case Right(i) => pos(disjointSets.values(i).last) //is already median position
        )*)

        val contractedBoxes = mappingContracted2Normal.map(normal => normal match
          case Left(i) => boxes.asRects(i)
          case Right(i) => disjointSets.values(i).map(v => boxes.asRects(v)).toSeq.sortBy(b => b.span.len).last
        )

        for i <- topSort do
          //test if there are any outgoing edges
          if contracedDiGraph.vertices(i.toInt).neighbors.length > 0 then
            // vertexbox of i
            //TODO: span.x1 is only valid for horizontal size 
            val box = contractedBoxes(i.toInt)          

            //val neighborLeftBoundary = contracedDiGraph.vertices(i.toInt).neighbors.map(n =>  contractedPos(n.toInt) - contractedBoxes(n.toInt).span).sortBy(v2d => -v2d.x1).last
            val neighborLeftBoundary = contracedDiGraph.vertices(i.toInt).neighbors
              .map(n => dir match
                case Direction.North => contractedPos(n.toInt) - contractedBoxes(n.toInt).span
                case Direction.East => contractedPos(n.toInt) - contractedBoxes(n.toInt).span
                case Direction.South => contractedPos(n.toInt) + contractedBoxes(n.toInt).span
                case Direction.West => contractedPos(n.toInt) + contractedBoxes(n.toInt).span
               )
              .sortBy(v2d => dir match
                case Direction.North => -v2d.x2
                case Direction.East => -v2d.x1
                case Direction.South => v2d.x2
                case Direction.West => v2d.x1
               )
              .last

            val newPos = dir match
              case Direction.West => Vec2D(neighborLeftBoundary.x1 + box.span.x1, contractedPos(i.toInt).x2)
              case Direction.East => Vec2D(neighborLeftBoundary.x1 - box.span.x1, contractedPos(i.toInt).x2)
              case Direction.North => Vec2D(contractedPos(i.toInt).x1, neighborLeftBoundary.x2 - box.span.x2)
              case Direction.South => Vec2D(contractedPos(i.toInt).x1, neighborLeftBoundary.x2 + box.span.x2)

            //update box positions
            contractedPos(i.toInt) = newPos

        //apply contracted positions to normal positions
        contractedPos.zipWithIndex.flatMap((p, i) => mappingContracted2Normal(i) match
          case Left(v) => Seq((v, p))
          case Right(set) => disjointSets.values(set).map(v => (v, p))
        ).foreach((v, p) => dir match
          case Direction.West | Direction.East => pos.update(v, Vec2D(p.x1,pos(v).x2))
          case Direction.North | Direction.South => pos.update(v, Vec2D(pos(v).x1, p.x2))
        )


    compactGraph(undirectedGraph, Direction.East, verticalSets)
    compactGraph(undirectedGraph, Direction.West, verticalSets)
    compactGraph(undirectedGraph, Direction.North, horizontalSets)
    compactGraph(undirectedGraph, Direction.South, horizontalSets)

    //rotate all points
    val alignedPos = pos.finish

    VertexLayout(alignedPos)
  end layout


end GreedyOrthogonalization
