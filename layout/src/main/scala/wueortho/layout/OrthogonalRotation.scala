// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout, WeightedGraph}
import wueortho.util.WhenSyntax.*


object OrthogonalRotation:
  
  
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
    
    //the list of the angles of the edges
    val alphaList = graph.edges
      .map(edge => (pos(edge.from.toInt), pos(edge.to.toInt)))
      .map((e1, e2) => math.atan(((e2.x2-e1.x2)/(e2.x1-e1.x1))))

    val sinSum: Double = alphaList
      .map(a => math.sin(a))
      .sum

    val cosSum: Double = alphaList
      .map(a => math.cos(a))
      .sum 

    val theta = -math.atan(sinSum/cosSum)

    val thetaList = LazyList.from(0).map(i => (math.Pi/4 * i - theta)).takeWhile(_.abs < math.Pi*2).toList
    
    //no minimum were found
    if thetaList.length == 0 then 
      println(s"WARN: no minimum was found for orthogonalization rotation equation")
      return VertexLayout(pos.finish)

    // function Sum_i sin^2(2*a_i/4+theta)
    val fx = (t: Double, a: Seq[Double]) => a.map(a => Math.sin(2* (a-t)))
      .map(x => x*x)
      .sum

    //computes the minimum and returns the corresponding angle
    val angleTuple =thetaList.map(t => fx(t, alphaList)).zipWithIndex.minBy(_._1)
      
    val angle = thetaList(angleTuple._2);

    // rotate point around origin by angle ccw
    def rotate(origin: Vec2D, point: Vec2D, angle: Double): Vec2D =
      val deltaP = point - origin
      val qx = origin.x1 + (Math.cos(angle) * deltaP.x1 - Math.sin(angle) * deltaP.x2)
      val qy = origin.x2 + (Math.sin(angle) * deltaP.x1 + Math.cos(angle) * deltaP.x2)
      Vec2D(qx, qy)
    end rotate

    //rotate all points
    val rotatedPoints = pos.finish.map(rotate(Vec2D(0.0, 0.0), _, -angle))

    VertexLayout(rotatedPoints)
  end layout


end OrthogonalRotation
