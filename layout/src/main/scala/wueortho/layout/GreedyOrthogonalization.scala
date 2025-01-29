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


object GreedyOrthogonalization:
  
  
  def layout(graph: WeightedGraph, init: VertexLayout): VertexLayout =
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

    var assignments: mutable.Seq[Map[Direction, Int]] = mutable.Seq()

    assignments = assignments.appendedAll(init.nodes.map(x => Map.empty[Direction, Int]))

    val allEdgeAngles = wueortho.data.mutable.Matrix.fill(n, n)(0)

    def addUndirectedEdges(edges: Seq[WeightedEdge]) = 
      edges.map(edge => (edge.from.toInt, edge.to.toInt))
      .map((from, to) => (from, to)::(to, from)::Nil)
      .flatMap(identity)

    def angleOfEdge(e1: Vec2D, e2: Vec2D) = 
       math.atan2((e2.x2-e1.x2),(e2.x1-e1.x1))

    val edges = graph.edges
    
    //TODO: assumes, that the graph is undirected. Will duplicate edges,
    val undirectedEdges = addUndirectedEdges(edges)

    undirectedEdges.map((from, to) => (from, to, pos(from), pos(to)))
      .foreach((from, to, e1, e2) => allEdgeAngles(from, to) = angleOfEdge(e1, e2))

    def directionToAngle(dir: Direction) = dir match
        case Direction.East => 0.0
        case Direction.North => Math.PI/2
        case Direction.West => Math.PI
        case Direction.South => -Math.PI/2

    def reverseDirection(dir: Direction) = dir match
      case Direction.East => Direction.West
      case Direction.North => Direction.South
      case Direction.West => Direction.East
      case Direction.South => Direction.North
    

    def edgeAlignmentCost(angle: Double, dir: Direction) = 
      val semiAxisAngle = directionToAngle(dir)
      Math.abs(angle - semiAxisAngle) min Math.abs(angle - Math.PI*2 - semiAxisAngle)


    val allEdges = addUndirectedEdges(graph.edges)

    val allEdgesGroupedByStart = allEdges
      .groupBy((from, _) => from)


    //TODO: Question: Does the order considers degree or only unassigned edges?
    //TODO: If only unassigned edges are considered, the ordering will change during assignement and cannot be precalulated in this way!
    val verticesOrderedByDegree = allEdgesGroupedByStart
      .map((from, l) => (from, l.length)).toSeq
      .sortBy((_, len) => len)
      .reverse

    def isCCW(dir: Direction, angle: Double) =
      val dirAngle = directionToAngle(dir)
      angle > dirAngle && (angle < dirAngle + Math.PI || Math.abs(angle - dirAngle) > Math.PI ) 
      true

    val allDirections = Seq(Direction.East, Direction.North, Direction.West, Direction.South)
    for vertex <- verticesOrderedByDegree do
      //calculate cost function for all edges starting at v
      var minCostsCCW = mutable.Map().addAll(allDirections.map(_ -> (Double.PositiveInfinity, (0,0))).toMap)
      var minCostsCW = mutable.Map().addAll(allDirections.map(_ ->  (Double.PositiveInfinity,(0,0))).toMap)

      if allEdgesGroupedByStart.get(vertex._1).isDefined then
        for v <- allEdgesGroupedByStart.get(vertex._1).get do
          for dir <- allDirections do
            val cost = edgeAlignmentCost(allEdgeAngles(v._1, v._2), dir)
            val relativeLocation = if(isCCW(dir, allEdgeAngles(v._1, v._2))) then minCostsCCW else minCostsCW
            if relativeLocation.getOrElse(dir, (Double.PositiveInfinity, (0,0)))._1 > cost && cost < Math.PI/4
              then if(isCCW(dir, allEdgeAngles(v._1, v._2))) 
                then minCostsCCW(dir) = (cost, v) 
                else minCostsCW(dir) = (cost, v)

      //assign edges
      var localAssignments = mutable.Map[Direction, (Int, Int)]()
      val allMinCosts = (minCostsCW ++ minCostsCCW).filter(x => x._2._1 != Double.PositiveInfinity).toSeq.sortBy(e => e._2._1)
      for candidate <- allMinCosts do
        //check for local assignment conflicts
        if !localAssignments.contains(candidate._1) && !localAssignments.values.toSeq.contains(candidate._2._2) then
          //Check for global assignment conflicts
          if !assignments(candidate._2._2._1).contains(candidate._1) && !assignments(candidate._2._2._2).contains(reverseDirection(candidate._1)) then
            localAssignments(candidate._1) = candidate._2._2
            //add nodes to disjoint sets
            val sets = candidate._1 match
              case Direction.North => verticalSets
              case Direction.South => verticalSets
              case Direction.West => horizontalSets
              case Direction.East => horizontalSets
            val edge = candidate._2._2
            if !sets.contains(edge._1) then
              sets.mkSet(edge._1,  Set(edge._1))
            if !sets.contains(edge._2) then
              sets.mkSet(edge._2,  Set(edge._2))
            sets.union(edge._1, edge._2)
        
      //save assignments for node
      assignments(vertex._1) = localAssignments.map(entry => (entry._1, entry._2._2)).toMap
      //with reverse direction
      localAssignments.toSeq.foreach(a => assignments(a._2._2) = assignments(a._2._2) + (reverseDirection(a._1) -> a._2._1))

    //calculate (median) positions for each disjoint set
    for v <- verticalSets.values if v.size > 0 do
      val median = v.map(v => pos(v).x1).toSeq.sorted()(Math.floor(v.size/2.0).toInt)
      v.foreach(v => pos.update(v,Vec2D(median, pos(v).x2)))

    for v <- horizontalSets.values if v.size > 0 do
      val median = v.map(v => pos(v).x2).toSeq.sorted()(Math.floor(v.size/2.0).toInt)
      v.foreach(v => pos.update(v, Vec2D(pos(v).x1, median)))

    println(s"vertical Sets: ${verticalSets.values.foldLeft("")((s,e) => s.concat(e.toString()).toString() )}")
    println(s"horizontal Sets: ${horizontalSets.values.foldLeft("")((s,e) => s.concat(e.toString()).toString() )}")

    //rotate all points
    val rotatedPoints = pos.finish

    VertexLayout(rotatedPoints)
  end layout


end GreedyOrthogonalization
