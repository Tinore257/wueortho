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

    val assignments: IndexedSeq[mutable.IndexedSeq[(Direction,(Int, Int))]] = IndexedSeq(init.nodes.map(_ => Direction.values.map(d => (d, (-1,-1))).sortBy((d, _) => d.ordinal))*)

    val allEdgeAngles = wueortho.data.mutable.Matrix.fill(n, n)(0)

    def angleOfEdge(e1: Vec2D, e2: Vec2D) = 
       math.atan2((e2.x2-e1.x2),(e2.x1-e1.x1))
    
    //TODO: assumes, that the graph is undirected. Will duplicate edges,
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

    val verticesOrderedByDegree = undirectedGraph.vertices
      .zipWithIndex
      .map((v, i) => (i, v.neighbors.length))
      .sortBy((_,l) => l)
      .reverse

    for (vertex, deg) <- verticesOrderedByDegree do
      //calculate cost function for all edges starting at v
      val minCosts: mutable.IndexedSeq[(Direction, Double, (Int, Int))] = Direction.values.map(d => (d, Double.PositiveInfinity, (0,0))).sortBy((d, _, _) => d.ordinal)

      //get for each direction edge within 45 degrees with minimal costs
      for neighbor <- undirectedGraph.vertices(vertex).neighbors.map(v => v.toNode.toInt) do
        for dir <- Direction.values do
          val cost = edgeAlignmentCost(allEdgeAngles(vertex, neighbor), dir)
          if minCosts(dir.ordinal)._2 > cost && math.abs(cost) < Math.PI/4
            then minCosts(dir.ordinal) = (dir, cost, (vertex, neighbor))
      
      //check for conflicts (i.e. double assignments, overrides) and maintain disjoint sets
      val localAssignments: mutable.IndexedSeq[(Direction, (Int, Int))] = Direction.values.map(d => (d, (-1,-1))).sortBy((d, _) => d.ordinal)
      val minCostsSorted = minCosts.filter(x => x._2 != Double.PositiveInfinity).toSeq.sortBy(e => e._2)
      for candidate <- minCostsSorted do 
        //check for local assignment conflicts
        if localAssignments(candidate._1.ordinal)._2 == (-1,-1) && !localAssignments.map(e => e._2).contains(candidate._3) then
          //Check for global assignment conflicts
          if assignments(candidate._3._1)(candidate._1.ordinal)._2 == (-1,-1) && assignments(candidate._3._2)(candidate._1.reverse.ordinal)._2 == (-1,-1) then
            localAssignments(candidate._1.ordinal) = (candidate._1, candidate._3)
            
            //add nodes to disjoint sets
            val sets = candidate._1 match
              case Direction.North | Direction.South => verticalSets
              case Direction.West | Direction.East => horizontalSets
            val edge = candidate._3
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
    for v <- verticalSets.values if v.size > 0 do
      val median = v.map(v => pos(v).x1).toSeq.sorted()(Math.floor(v.size/2.0).toInt)
      v.foreach(v => pos.update(v,Vec2D(median, pos(v).x2)))

    for v <- horizontalSets.values if v.size > 0 do
      val median = v.map(v => pos(v).x2).toSeq.sorted()(Math.floor(v.size/2.0).toInt)
      v.foreach(v => pos.update(v, Vec2D(pos(v).x1, median)))

    println(s"vertical Sets: ${verticalSets.values.foldLeft("")((s,e) => s.concat(e.toString()).toString() )}")
    println(s"horizontal Sets: ${horizontalSets.values.foldLeft("")((s,e) => s.concat(e.toString()).toString() )}")

    //TODO: hierarchical ordering

    //rotate all points
    val rotatedPoints = pos.finish

    VertexLayout(rotatedPoints)
  end layout


end GreedyOrthogonalization
