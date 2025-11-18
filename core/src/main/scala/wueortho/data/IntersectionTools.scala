package wueortho.data

case class IntersectionTools():

  def intersect(e1: AlignedEdge, e2: AlignedEdge, pos: VertexLayout): Boolean =
    intersect(SimpleEdge(e1.from, e1.to), SimpleEdge(e2.from, e2.to), pos)
  end intersect

  def intersect(e1: SimpleEdge, e2: SimpleEdge, pos: VertexLayout): Boolean =

    // test for shared endpoint
    if Seq(e1.from, e1.to, e2.from, e2.to).map(id => pos(id)).combinations(2).exists(_.reduce(_ - _).len == 0)
    then return false

    def orientationTest(e: SimpleEdge, node: NodeIndex): Double =
      val p = pos(e.from)
      val q = pos(e.to)
      val r = pos(node)
      val v = (q.x2 - p.x2) * (r.x1 - q.x1) -
        (q.x1 - p.x1) * (r.x2 - q.x2)
      if Math.abs(v) <= 0.00001 then v else v.sign

    def onSegment(p1: NodeIndex, p2: NodeIndex, node: NodeIndex) =
      val p = pos(p1)
      val q = pos(p2)
      val r = pos(node)
      q.x1 <= (p.x1 max r.x1) && q.x1 >= (p.x1 min r.x1) &&
      q.x2 <= (p.x2 max r.x2) && q.x2 >= (p.x2 min r.x2)

    val o1 = orientationTest(e1, e2.from)
    val o2 = orientationTest(e1, e2.to)
    val o3 = orientationTest(e2, e1.from)
    val o4 = orientationTest(e2, e1.to)

    // general case
    if (o1 != o2 && o3 != o4) then
      println(s"Edege (${e1.from},${e1.to}) intersects (${e2.from}, ${e2.to}) with positions (${pos(e1.from)
          .toString}, ${pos(e1.to).toString}) and (${pos(e2.from).toString}, ${pos(e2.to).toString})")
      return true

    return false
    // edge-cases with colinearity
    if (o1 == 0 && onSegment(e1.from, e2.from, e1.to)) then
      println(s"Edege (${e1.from},${e1.to}) intersects (${e2.from}, ${e2.to}) ")
      return true
    if (o2 == 0 && onSegment(e1.from, e2.to, e1.to)) then
      println(s"Edege (${e1.from},${e1.to}) intersects (${e2.from}, ${e2.to}) ")
      return true
    if (o3 == 0 && onSegment(e2.from, e1.from, e2.to)) then
      println(s"Edege (${e1.from},${e1.to}) intersects (${e2.from}, ${e2.to}) ")
      return true
    if (o4 == 0 && onSegment(e2.from, e1.to, e2.to)) then
      println(s"Edege (${e1.from},${e1.to}) intersects (${e2.from}, ${e2.to}) ")
      return true
    false
  end intersect
end IntersectionTools
