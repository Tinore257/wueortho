// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import _root_.wueortho.data.Vec2D
import _root_.wueortho.data.WeightedEdge
import _root_.wueortho.util.Vec2D.normalized
import _root_.wueortho.util.Vec2D.perpendicular

object EdgeUtils:

    def intersectRaw(a1: Vec2D, a2: Vec2D, b1: Vec2D, b2: Vec2D): Boolean =

      def orientationTest(e1: Vec2D, e2: Vec2D, node: Vec2D): Double =
        val v = (e2.x2 - e1.x2) * (node.x1 - e2.x1) -
          (e2.x1 - e1.x1) * (node.x2 - e2.x2)
        if Math.abs(v) <= 0.00001 then v else v.sign

      def onSegment(p1: Vec2D, p2: Vec2D, node: Vec2D) =
        p2.x1 <= (p1.x1 max node.x1) && p2.x1 >= (p1.x1 min node.x1) &&
          p2.x2 <= (p1.x2 max node.x2) && p2.x2 >= (p1.x2 min node.x2)

      val o1 = orientationTest(a1, a2, b1)
      val o2 = orientationTest(a1, a2, b2)
      val o3 = orientationTest(b1, b2, a1)
      val o4 = orientationTest(b1, b2, a2)

      // general case
      if (o1 != o2 && o3 != o4) then return true

      return false
      // edge-cases with colinearity
      if (o1 == 0 && onSegment(a1, b1, a2)) then return true
      if (o2 == 0 && onSegment(a1, b2, a2)) then return true
      if (o3 == 0 && onSegment(b1, a1, b2)) then return true
      if (o4 == 0 && onSegment(b1, a2, b2)) then return true
      false
    end intersectRaw

    def intersect(e1: WeightedEdge, e2: WeightedEdge, pos: Seq[Vec2D]): Boolean =
      // test for shared endpoint
      if pos(e1.from.toInt) != pos(e1.to.toInt) && pos(e1.to.toInt) != pos(e2.to.toInt) then
        if (pos(e1.from.toInt) :: pos(e1.to.toInt) :: pos(e2.from.toInt) :: pos(e2.to.toInt) :: Nil).permutations
            .map(_.take(2).reduce(_ - _).len == 0).reduce(_ || _)
        then return false
      intersectRaw(pos(e1.from.toInt), pos(e1.to.toInt), pos(e2.from.toInt), pos(e2.to.toInt))
    end intersect

    /**
      * calculates the distance from a1 to an intersecion with the segment b1-b2 if there is any intersetion
      *
      * @param a1 origin of the ray
      * @param a2 direction of the ray starting from a1
      * @param b1 segment b start point
      * @param b2 segment b end point
      * @return Option of the distance to the intersection point, None, if there is no intersection
      */
    def intersectionDistance(a1: Vec2D, a2: Vec2D, b1: Vec2D, b2: Vec2D): Option[Double] = 
      if !intersectRaw(a1, a2, b1, b2) then return Option.empty else 
        val n =  normalized(perpendicular(b1-b2))
        val d = normalized(a1-b1)
        val t = (b1.dot(n)-a1.dot(n))/(d.dot(n))
        return Some(t)

    /**
      * calculates the an Option of the position of two segments intersecting, None if there is no intersection 
      *
      * @param a1 segment A start point
      * @param a2 segment A end point
      * @param b1 segment B start point
      * @param b2 segment B end point
      * @return Option containing the intersection point if present
      */
    def intersectionPoint(a1: Vec2D, a2: Vec2D, b1: Vec2D, b2: Vec2D): Option[Vec2D] =
      intersectionDistance(a1, a2, b1, b2).map(t => normalized(a2-a1).scale(t))
          
          
          
end EdgeUtils
          
object Vec2D:
  def perpendicular(vector: Vec2D):Vec2D = 
    new Vec2D(-vector.x2, vector.x1)

  def normalized(vector: Vec2D): Vec2D = 
    vector.scale(1.0/vector.len)
end Vec2D

implicit class Vec2DAlgebra(val value: Vec2D) extends AnyVal {
    def dot(other: Vec2D) = value.x1 * other.x1 + value._2 * other.x2 
}