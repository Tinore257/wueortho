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

object SGDStressMinimization:


//Floyd Warshall Algorithm in GraphSearch.scala (floydWarshallApsp)
  def layout(cfg: Config)(graph: WeightedGraph, init: VertexLayout): VertexLayout =
    val dij: MatrixView[Double] = floydWarshallApsp(graph.numberOfVertices, graph.edges)
    val n = graph.numberOfVertices

    class PosVec(init: Seq[Vec2D]):
      var a = Array[Vec2D](init*)

      def apply(i: Int) = a(i)
      //def delta(i: Int) = a(i) - b(i)
      def finish        = a.toVector

      def addStress(i: Int, delta: Vec2D)   = a(i) += delta
      def update(i: Int, value: Vec2D) = a(i) = value

      //def applyChanges() = Array.copy(a, 0, b, 0, a.length)
    end PosVec
      
    @tailrec
    def go(i: Int, etaMax: Double, pos: PosVec): Vector[Vec2D] =
      if i < 0 then pos.finish
      else

        //get permutation of nodes
        val list: Seq[Int] = Range.inclusive(0, n-1)
        val permutation: Seq[Int] = scala.util.Random.shuffle(list)

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

          val stepSize = cfg.eta(i, etaMax)

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

        go(i - 1, etaMax, pos)
    end go

    var maxDiameter = 0.0
    var minDiameter = Double.PositiveInfinity

    for
      k <- 0 until n
      j <- (k + 1) until n
    do
      val d = dij(k, j)
      maxDiameter = if maxDiameter < d && d != Double.PositiveInfinity then d else maxDiameter
      minDiameter = if minDiameter > d then d else minDiameter

    val epsilon = 0.03
    val etaMax = minDiameter * minDiameter
    val etaMin = epsilon * maxDiameter * maxDiameter 

    VertexLayout(go(cfg.iterCap, etaMax, PosVec(init.nodes)))
  end layout

  case class Config(
      startingStepSize: Double,
      iterCap: Int,
      eta: (Int, Double) => Double,
  )

  val defaultConfig = Config(
    startingStepSize = 0.5,
    iterCap = 0,
    // cooling = x => (x - 0.1) * 0.995 + 0.1,
    //cooling = x => (x - 0.02) max 0.01,
    eta = (t, etaMax) => (etaMax * Math.exp(-0.03 * t)) 
  )

  def initLayout(rand: Random, n: Int) =
    VertexLayout(Vector.fill(n)(Vec2D(rand.nextGaussian() * sqrt(n), rand.nextGaussian() * sqrt(n))))

end SGDStressMinimization
