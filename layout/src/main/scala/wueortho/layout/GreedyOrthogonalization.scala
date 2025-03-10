// SPDX-FileCopyrightText: 2024 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.layout

import wueortho.data.{Vec2D, VertexLayout, WeightedGraph}
import wueortho.data.Direction
import scala.collection.mutable
import wueortho.data.WeightedEdge
import wueortho.util.mutable.DisjointSets
import wueortho.util.Monoid
import wueortho.data.NodeIndex
import wueortho.util.GraphConversions.wg2wd
import wueortho.data.Graph
import wueortho.data.SimpleEdge
import wueortho.data.VertexBoxes
import wueortho.data.WeightedDiGraph
import wueortho.util.mutable.LinearIntervalTree.Interval
import wueortho.util.mutable.LinearIntervalTree

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

    given Monoid[Set[NodeIndex]] with
      def zero: Set[NodeIndex]                                                 = Set.empty
      override def apply(a: Set[NodeIndex], b: Set[NodeIndex]): Set[NodeIndex] = a union b

    val verticalSets   = DisjointSets[NodeIndex, Set[NodeIndex]]
    val horizontalSets = DisjointSets[NodeIndex, Set[NodeIndex]]

    Range(0, graph.numberOfVertices).foreach(i => verticalSets.mkSet(NodeIndex(i), Set(NodeIndex(i))))
    Range(0, graph.numberOfVertices).foreach(i => horizontalSets.mkSet(NodeIndex(i), Set(NodeIndex(i))))

    val assignments: IndexedSeq[mutable.IndexedSeq[(Direction, (Int, Int))]] = IndexedSeq(
      init.nodes.map(_ => Direction.values.map(d => (d, (-1, -1))).sortBy((d, _) => d.ordinal))*,
    )

    val allEdgeAngles = wueortho.data.mutable.Matrix.fill(n, n)(0)

    def angleOfEdge(e1: Vec2D, e2: Vec2D) =
      math.atan2((e2.x2 - e1.x2), (e2.x1 - e1.x1))

    // TODO: assumes, that graph is undirected weighted graph
    val undirectedGraph = wg2wd(graph)

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

    def intersect(e1: WeightedEdge, e2: WeightedEdge): Boolean =

      // test for shared endpoint
      if pos(e1.from.toInt) != pos(e1.to.toInt) && pos(e2.from.toInt) != pos(e2.to.toInt) then
        if (e1.from :: e1.to :: e2.from :: e2.to :: Nil).map(id => pos(id.toInt)).permutations
            .map(_.take(2).reduce(_ - _).len == 0).reduce(_ || _)
        then return false

      def orientationTest(e: WeightedEdge, node: NodeIndex): Double =
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

    def isAligned(e: WeightedEdge, dir: Direction) = dir match
      case Direction.East | Direction.West   =>
        // horizontalSets.contains(e.from) && horizontalSets.contains(e.to)
        (horizontalSets.contains(e.from) && horizontalSets.getOrElseThrow(e.from).contains(e.to))
      case Direction.North | Direction.South => // verticalSets.contains(e.from) && verticalSets.contains(e.to)
        (verticalSets.contains(e.from) && verticalSets.getOrElseThrow(e.from).contains(e.to))

    // TODO: edge-case class
    case class Candidate(dir: Direction, weight: Double, edge: (Int, Int))

    for (vertex, deg) <- verticesOrderedByDegree do
      // calculate cost function for all edges starting at v
      val minCosts: mutable.IndexedSeq[Candidate] = Direction.values
        .map(d => Candidate(d, Double.PositiveInfinity, (0, 0))).sortBy(_.dir.ordinal)

      // get for each direction edge within 45 degrees with minimal costs
      for neighbor <- undirectedGraph.vertices(vertex).neighbors.map(v => v.toNode.toInt) do
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
                  WeightedEdge(NodeIndex(candidate.edge._1), NodeIndex(candidate.edge._2), 1.0),
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
      end for
      // save assignments for node
      localAssignments.filter(e => e._2 != (-1, -1)).foreach(a => assignments(a._2._1)(a._1.ordinal) = a)
      // with reverse direction
      localAssignments.filter(e => e._2 != (-1, -1))
        .foreach(a => assignments(a._2._2)(a._1.reverse.ordinal) = (a._1, (a._2._2, a._2._1)))
    end for
    // calculate (median) positions for each disjoint set and set nodes position to median
    for vertices      <- verticalSets.values if vertices.size > 0 do
      val medianSeq = vertices.toSeq.map(v => pos(v.toInt).x1).sorted
      val median    = medianSeq(Math.floor(vertices.size.toDouble / 2.0).toInt)
      vertices.foreach(v => pos.update(v.toInt, Vec2D(median, pos(v.toInt).x2)))

    for v <- horizontalSets.values if v.size > 0 do
      val median = v.toSeq.map(v => pos(v.toInt).x2).sorted()(Math.floor(v.size / 2.0).toInt)
      v.foreach(v => pos.update(v.toInt, Vec2D(pos(v.toInt).x1, median)))

    println(s"vertical Sets: ${verticalSets.values.foldLeft("")((s, e) => s.concat(e.toString()).toString())}")
    println(s"horizontal Sets: ${horizontalSets.values.foldLeft("")((s, e) => s.concat(e.toString()).toString())}")

    def isVertical(dir: Direction) = dir match
      case Direction.East | Direction.West => false
      case _                               => true

    def isEarlierInSweepDir(pos: Vec2D, otherPos: Vec2D, dir: Direction): Boolean = dir match
      case Direction.East  => pos.x1 < otherPos.x1
      case Direction.West  => pos.x1 > otherPos.x1
      case Direction.North => pos.x2 < otherPos.x2
      case Direction.South => pos.x2 > otherPos.x2

    /** Returns additional edges between nodes that need to be added to the according nodes in the confict-graph
      */
    def sweeplineDetectConflicts(
        dir: Direction,
        pos: IndexedSeq[Vec2D],
    ): Seq[SimpleEdge] =
      // return the edges that need to be added to the conflict graph
      var additionalEdges = mutable.Seq[SimpleEdge]()

      val orthoDir = dir match
        case Direction.North | Direction.South => Direction.East
        case Direction.West | Direction.East   => Direction.North

      val sets = dir match
        case Direction.East | Direction.West   => verticalSets
        case Direction.South | Direction.North => horizontalSets

      // sweepPos is the position in the direction of sweeping
      case class Segment(sweepPos: Double, low: Double, high: Double, ref: Int) derives CanEqual

      // init sweepline status
      val sweeplineStatus = LinearIntervalTree()

      // get all aligned edges (parallel to sweeping line)
      val alignedEdges = graph.edges.filter(e => isAligned(e, orthoDir))

      val isolatedNodes = graph.vertices.indices.filter(v => (sets.getOrElseThrow(NodeIndex(v)).size <= 1))

      // init event queue
      val segmentsFromAlignedEdges = isVertical(dir) match
        case false =>
          alignedEdges.map(e =>
            Segment(
              pos(e.from.toInt).x1 min pos(e.to.toInt).x1,
              pos(e.from.toInt).x2 min pos(e.to.toInt).x2,
              pos(e.from.toInt).x2 max pos(e.to.toInt).x2,
              if isEarlierInSweepDir(pos(e.from.toInt), pos(e.to.toInt), dir) then e.from.toInt else e.to.toInt,
            ),
          )
        case true  =>
          alignedEdges.map(e =>
            Segment(
              pos(e.from.toInt).x2 min pos(e.to.toInt).x2,
              pos(e.from.toInt).x1 min pos(e.to.toInt).x1,
              pos(e.from.toInt).x1 max pos(e.to.toInt).x1,
              if isEarlierInSweepDir(pos(e.from.toInt), pos(e.to.toInt), dir) then e.from.toInt else e.to.toInt,
            ),
          )

      val segmentsFromIsolatedVertices = isVertical(dir) match
        case false => isolatedNodes.map(v => Segment(pos(v).x1, pos(v).x2, pos(v).x2, v))
        case true  => isolatedNodes.map(v => Segment(pos(v).x2, pos(v).x1, pos(v).x1, v))

      // PROBLEM: HIER LEIGEN DIE NODEINDEXE NOCH IN GLOBALER FORM VOR

      val eventQueue = segmentsFromIsolatedVertices.appendedAll(segmentsFromAlignedEdges).sortBy(dir match
        case Direction.East | Direction.South => _.sweepPos
        case Direction.West | Direction.North => -_.sweepPos,
      ).map(s => Interval(s.low, s.high, s.ref))

      for currentInterval <- eventQueue do
        // finde interval in BBST
        val interval = sweeplineStatus.overlaps(currentInterval.low, currentInterval.high)

        // all intersecting intervals that are not in a disjoint set with the current interval
        val problemVertices = interval.filter(i => !sets.sameSet(NodeIndex(i), NodeIndex(currentInterval.key)))

        additionalEdges = additionalEdges.appendedAll(
          problemVertices.map(v => SimpleEdge(NodeIndex(v), NodeIndex(currentInterval.key))).distinct,
        )

        // replace interval
        sweeplineStatus.cutout(currentInterval.low, currentInterval.high)

        // add current Interval to sweepline
        sweeplineStatus.+=(currentInterval.low, currentInterval.high, currentInterval.key)

      end for

      return additionalEdges.toSeq
    end sweeplineDetectConflicts

    def compactGraph(graph: WeightedDiGraph, dir: Direction, disjointSets: DisjointSets[NodeIndex, Set[NodeIndex]]) =

      def getContracted = (n: NodeIndex) => {
        disjointSets.getOrElseThrow(n).toSeq.sortBy(v => -v.toInt).last
      }

      def getUnContracted = (repr: NodeIndex) => {
        disjointSets.getOrElseThrow(repr)
      }

      // graph with vertial aligned vertices contracted
      val initialContracedDiGraph = Graph.fromEdges(
        graph.edges.flatMap(e =>
          Seq(
            SimpleEdge(
              getContracted(e.from),
              getContracted(e.to),
            ),
            SimpleEdge(getContracted(e.to), getContracted(e.from)),
          ),
        ).filter(e => isEarlierInSweepDir(pos(e.from.toInt), pos(e.to.toInt), dir)).distinct,
        graph.numberOfVertices,
      ).mkDiGraph

      // println(s"contracted graph ${contracedDiGraph.vertices.zipWithIndex.map((e,i) => s"${i} -> ${e.toString()} \n" ).toString()}")

      val conflictEdges = sweeplineDetectConflicts(dir, pos.finish)

      println(s"Added ${conflictEdges.size.toString()} conflict edges")

      val contracedDiGraph = Graph.fromEdges(
        initialContracedDiGraph.edges
          .appendedAll(conflictEdges.map(e => SimpleEdge(getContracted(e.from), getContracted(e.to)))).distinct,
        graph.numberOfVertices,
      ).mkDiGraph

      // topological ordering of the (contracted) nodes
      val topSort = topologicalSort(
        contracedDiGraph.vertices.zipWithIndex.map((_, i) => NodeIndex(i)),
        x => contracedDiGraph.vertices(x.toInt).neighbors.map(getContracted).distinct,
      )

      for i <- topSort do
        // (outgoing) neighbors
        val conflictNeighbors = contracedDiGraph.vertices(i.toInt).neighbors

        if conflictNeighbors.size > 0 then

          val neighbors = conflictNeighbors.map(disjointSets.getOrElseThrow(_))

          val neighborsWithPosBox = neighbors.flatMap(l => l.map(n => (n, pos(n.toInt), boxes(n.toInt))))

          val neighborsBoundary = (dir match
            case Direction.West  => neighborsWithPosBox.sortBy((_, pos, box) => -pos.x1 - box.span.x1)
            case Direction.East  => neighborsWithPosBox.sortBy((_, pos, box) => pos.x1 + box.span.x1)
            case Direction.North => neighborsWithPosBox.sortBy((_, pos, box) => pos.x2 + box.span.x2)
            case Direction.South => neighborsWithPosBox.sortBy((_, pos, box) => -pos.x2 - box.span.x2)
          ).reverse.last

          // bounding box to add to neightbors boundary to get final position of vertices
          val currentBoundingBox = disjointSets.getOrElseThrow(i).map(v => boxes(v.toInt)).toSeq.sortBy(box =>
            (dir match
              case Direction.West | Direction.East   => box.span.x1
              case Direction.North | Direction.South => box.span.x2),
          ).last

          val newPos = dir match
            case Direction.West  => neighborsBoundary._2.x1 + neighborsBoundary._3.span.x1 + currentBoundingBox.span.x1
            case Direction.East  => neighborsBoundary._2.x1 - neighborsBoundary._3.span.x1 - currentBoundingBox.span.x1
            case Direction.North => neighborsBoundary._2.x2 - neighborsBoundary._3.span.x2 - currentBoundingBox.span.x2
            case Direction.South => neighborsBoundary._2.x2 + neighborsBoundary._3.span.x2 + currentBoundingBox.span.x2

          getUnContracted(i).foreach(v =>
            dir match
              case Direction.West | Direction.East   => pos.update(v.toInt, Vec2D(newPos, pos(v.toInt).x2))
              case Direction.North | Direction.South => pos.update(v.toInt, Vec2D(pos(v.toInt).x1, newPos)),
          )
        end if
      end for

    end compactGraph

    // compactGraph(undirectedGraph, Direction.East, verticalSets)
    compactGraph(undirectedGraph, Direction.West, verticalSets)
    // compactGraph(undirectedGraph, Direction.North, horizontalSets)
    compactGraph(undirectedGraph, Direction.South, horizontalSets)

    // pos.a.zipWithIndex.foreach((p, i) => println(s"Vertex: ${i} has position  ${p.toString()}"))

    // rotate all points
    val alignedPos = pos.finish

    VertexLayout(alignedPos)
  end layout

end GreedyOrthogonalization
