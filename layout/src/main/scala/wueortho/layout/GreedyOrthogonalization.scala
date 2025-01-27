// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout, WeightedGraph}
import java.security.Identity
import scala.compiletime.ops.float
import wueortho.data.Direction
import scala.collection.mutable
import wueortho.data.WeightedEdge
import wueortho.util.mutable.DisjointSets
import wueortho.util.Monoid
import wueortho.util.GraphConversions.all
import scala.compiletime.ops.double
import scala.collection.mutable.HashMap
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


    var verticalSets = DisjointSets[Int, Set[Int]]
    var horizontalSets = DisjointSets[Int, Set[Int]]

    var assignments: mutable.Seq[Map[Direction, Int]] = mutable.Seq()

    assignments = assignments.appendedAll(init.nodes.map(x => Map.empty[Direction, Int]))

    val allEdgeAngles = wueortho.data.mutable.Matrix.fill(n, n)(0)

    def angleOfEdge(e1: Vec2D, e2: Vec2D) = 
       math.atan(((e2.x2-e1.x2)/(e2.x1-e1.x1)))

    //TODO: assumes, that the graph is undirected. Will duplicate edges,
    graph.edges
      .map(edge => (edge.from.toInt, edge.to.toInt))
      .map((from, to) => (from, to)::(to, from)::Nil)
      .flatMap(identity)
      .map((from, to) => (from, to, pos(from), pos(to)))
      .foreach((from, to, e1, e2) => allEdgeAngles(from, to) = angleOfEdge(e1, e2))

    def directionToAngle(dir: Direction) = dir match
        case Direction.East => 0.0
        case Direction.North => Math.PI/2
        case Direction.West => Math.PI
        case Direction.South => Math.PI*3/2

    def reverseDirection(dir: Direction) = dir match
      case Direction.East => Direction.West
      case Direction.North => Direction.South
      case Direction.West => Direction.East
      case Direction.South => Direction.North
    

    def edgeAlignmentCost(angle: Double, dir: Direction) = 
      val semiAxisAngle = directionToAngle(dir)
      Math.abs(angle - semiAxisAngle) min Math.abs(angle + 360 - semiAxisAngle)

    def addUndirectedEdges(edges: Seq[WeightedEdge]) = 
      edges.map(edge => (edge.from.toInt, edge.to.toInt))
      .map((from, to) => (from, to)::(to, from)::Nil)
      .flatMap(identity)

    val allEdges = addUndirectedEdges(graph.edges)

    val allEdgesGroupedByStart = allEdges
      .groupBy((from, _) => from)


    //TODO: Question: Does the order considers degree or only unassigned edges?
    //TODO: If only unassigned edges are considered, the ordering will change during assignement and cannot be precalulated in this way!
    val verticesOrderedByDegree = allEdgesGroupedByStart
      .flatMap((from, l) => Seq((from, l.length))).toSeq
      .sortBy((from, l) => l)
      .reverse

    def isCCW(dir: Direction, angle: Double) =
      val dirAngle = directionToAngle(dir)
      angle % (Math.PI * 2) < dirAngle

    val allDirections = Seq(Direction.East, Direction.North, Direction.West, Direction.South)
    for v <- verticesOrderedByDegree do
      //calculate cost function for all edges starting at v
      val minCostsCCW = allDirections.map(_ -> (Double.PositiveInfinity, (0,0))).toMap
      val minCostsCW = allDirections.map(_ ->  (Double.PositiveInfinity,(0,0))).toMap

      if allEdgesGroupedByStart.get(v._1).isDefined then
        for dir <- allDirections do
          val cost = edgeAlignmentCost(allEdgeAngles(v._1, v._2), dir)
          var relativeLocation = if(isCCW(dir, allEdgeAngles(v._1, v._1))) then minCostsCCW else minCostsCW
          relativeLocation = if relativeLocation.getOrElse(dir, (Double.PositiveInfinity, (0,0)))._1 < cost 
            then relativeLocation 
            else relativeLocation + (dir -> (cost, v))

      //assign edges
      var localAssignments = new HashMap[Direction, (Int, Int)]
      val allMinCosts = (minCostsCW ++ minCostsCCW).toSeq.sortBy(e => e._2._1)
      for candidate <- allMinCosts do
        //check for local assignment conflicts
        if !localAssignments.contains(candidate._1) && !localAssignments.values.toSeq.contains(candidate._2._2) then
          //Check for global assignment conflicts
          if !assignments(candidate._2._2._1).contains(candidate._1) && !assignments(candidate._2._2._2).contains(reverseDirection(candidate._1)) then
            localAssignments = localAssignments + (candidate._1 -> candidate._2._2)
            //add nodes to disjoint sets
            val sets = candidate._1 match
              case Direction.North => verticalSets
              case Direction.South => verticalSets
              case Direction.West => horizontalSets
              case Direction.East => horizontalSets
            val edge = candidate._2._2
            if !sets.contains(edge._1) then
              sets.mkSet(edge._1,  Set.empty[Int])
            if !sets.contains(edge._2) then
              sets.mkSet(edge._2,  Set.empty[Int])
            sets.union(edge._1, edge._2)
        
      //enforce assignments
      assignments(v._1) = localAssignments.map(entry => (entry._1, entry._2._2)).toMap
      //with reverse direction
      assignments(v._2) = localAssignments.map(entry => (reverseDirection(entry._1), entry._2._1)).toMap


    //rotate all points
    val rotatedPoints = pos.finish

    VertexLayout(rotatedPoints)
  end layout


end GreedyOrthogonalization
