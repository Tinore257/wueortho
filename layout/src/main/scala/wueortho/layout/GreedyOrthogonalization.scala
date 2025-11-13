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

object GreedyOrthogonalization:

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

    def angleOfEdge(e1: Vec2D, e2: Vec2D) =
      math.atan2((e2.x2 - e1.x2), (e2.x1 - e1.x1))

    // TODO: assumes, that graph is undirected weighted graph
    val undirectedGraph = sg2dg(graph)

    val allEdges = undirectedGraph.edges

    // precalculate all edge angles
    allEdges.map(b => (b.from, b.to, pos(b.from.toInt), pos(b.to.toInt)))
      .foreach((from, to, e1, e2) => allEdgeAngles(from.toInt, to.toInt) = angleOfEdge(e1, e2))

    def directionToAngle(dir: Direction) = dir match
      case Direction.East  => 0.0
      case Direction.North => Math.PI / 2
      case Direction.West  => Math.PI
      case Direction.South => -Math.PI / 2

    def edgeAlignmentCost(angle: Double, dir: Direction) =
      val semiAxisAngle = directionToAngle(dir)
      Math.abs(angle - semiAxisAngle) min Math.abs(angle - Math.PI * 2 - semiAxisAngle)

    def intersect(e1: SimpleEdge, e2: SimpleEdge): Boolean =

      // test for shared endpoint
      if pos(e1.from.toInt) != pos(e1.to.toInt) && pos(e2.from.toInt) != pos(e2.to.toInt) then
        if (e1.from :: e1.to :: e2.from :: e2.to :: Nil).map(id => pos(id.toInt)).permutations
            .map(_.take(2).reduce(_ - _).len == 0).reduce(_ || _)
        then return false

      def orientationTest(e: SimpleEdge, node: NodeIndex): Double =
        val p = pos(e.from.toInt)
        val q = pos(e.to.toInt)
        val r = pos(node.toInt)
        val v = (q.x2 - p.x2) * (r.x1 - q.x1) -
          (q.x1 - p.x1) * (r.x2 - q.x2)
        if Math.abs(v) <= 0.00001 then v else v.sign

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

      // general case
      if (o1 != o2 && o3 != o4) then
        println(s"Edege (${e1.from},${e1.to}) intersects (${e2.from}, ${e2.to}) with positions (${pos(e1.from.toInt)
            .toString}, ${pos(e1.to.toInt).toString}) and (${pos(e2.from.toInt).toString}, ${pos(e2.to.toInt).toString})")
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
                !(isAligned(e, candidate.dir) && intersect(
                  e,
                  SimpleEdge(NodeIndex(candidate.edge._1), NodeIndex(candidate.edge._2)),
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
    val alignedGraph = AlignedGraph.fromAlignedEdges(alingedEdges.toSeq).mkAlignedGraph

    alignedGraph
  end greedyAlignedGraph

end GreedyOrthogonalization
