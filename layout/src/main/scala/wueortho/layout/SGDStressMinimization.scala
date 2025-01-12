// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout, WeightedGraph}
import wueortho.util.WhenSyntax.*

import scala.annotation.tailrec
import scala.util.Random
import java.lang.Math.sqrt
import wueortho.data.mutable.MatrixView
import wueortho.util.GraphSearch
import wueortho.util.GraphSearch.floydWarshallApsp
import wueortho.data.WeightedEdge

object SGDStressMinimization:
  
  
  def layout(cfg: Config)(rand: Random, graph: WeightedGraph, init: VertexLayout): VertexLayout =
    val undirectedEdges = graph.edges.flatMap(e => Seq(WeightedEdge(e.from, e.to, e.weight), WeightedEdge(e.to, e.from, e.weight)))
    val dij: MatrixView[Double] = floydWarshallApsp(graph.numberOfVertices, undirectedEdges)
    val n = graph.numberOfVertices
    
    var dMax = 0.0
    //var dMin = Double.PositiveInfinity
    
    for
      k <- 0 until n
      j <- (k + 1) until n
    do
      val d = dij(k, j)
      dMax = if dMax < d && d != Double.PositiveInfinity then d else dMax
      //dMin = if dMin > d then d else dMin
         
      
    val etaMin = 0.1
    val cMax = dMax * dMax

    val anealing = cfg.eta(cMax, etaMin, cfg.iterCap)
    
    class PosVec(init: Seq[Vec2D]):
      var a = Array[Vec2D](init*)

      def apply(i: Int) = a(i)
      def finish        = a.toVector

      def update(i: Int, value: Vec2D) = a(i) = value

    end PosVec
      
    @tailrec
    def go(i: Int, pos: PosVec): Vector[Vec2D] =
      if i < 0 then pos.finish
      else

        //get permutation of nodes
        val list: Seq[Int] = Range.inclusive(0, n-1)
        val permutation: Seq[Int] = rand.shuffle(list)
        //val permutation: Seq[Int] = Random.shuffle(list)
        
        // calculate stress:
        for
          k <- 0 until n
          j <- (k + 1) until n
        do
          //get random node
          val u = permutation(k)
          val v = permutation(j)

          //val duv = if dij(u, v) != Double.PositiveInfinity then dij(u, v)  else 0.0
          val duv = dij(u, v)
          val wij = 1.0/(duv * duv)

          val stepSize =  anealing(cfg.iterCap - i) //cfg.eta(cfg.iterCap - i, etaMax, etaMin, cfg.iterCap)

          val clampedStepSize = (wij * stepSize) min 1.0

          //magnitude/distance between nodes
          val deltaX = (pos(u) - pos(v)).len

          //caculate r scalar
          val r:Vec2D =  (pos(u) - pos(v)).scale(1.0/deltaX).scale( (deltaX - duv)/2 )

          //only apply when stepsize is not null
          // stepsize may be null, if u and v are not connected => displacement is infinity
          if(clampedStepSize != 0)
            val displacement = r.scale(clampedStepSize);
            //move u and v by r
            pos(u) = pos(u) - displacement
            pos(v) = pos(v) + displacement

        go(i - 1, pos)
    end go

    VertexLayout(go(cfg.iterCap, PosVec(init.nodes)))
  end layout

  case class Config(
      startingStepSize: Double,
      iterCap: Int,
      //eta: (Int, Double, Double, Int) => Double,
      eta: (Double, Double, Int) => (Int => Double), 
  )

  val defaultConfig = Config(
    startingStepSize = 0.5,
    iterCap = 1000,
    //eta = (t, etaMax, etaMin, iterCap) => etaMax * Math.exp(t * (Math.log(etaMin/etaMax))/(iterCap - 1))
    eta = (etaMax, etaMin, iterCap) => (t =>  etaMax * Math.exp(t * (Math.log(etaMin/etaMax))/(iterCap - 1)))
  )

  def initLayout(rand: Random, n: Int) =
    VertexLayout(Vector.fill(n)(Vec2D(rand.nextGaussian() * sqrt(n), rand.nextGaussian() * sqrt(n))))

end SGDStressMinimization
