// SPDX-FileCopyrightText: 2025 Tino Reith <tino.reith@stud-mail.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0
package wueortho.layout

import wueortho.data.*
import scala.collection.mutable
import wueortho.data.AlignedEdge
import wueortho.util.GraphSearch.bfs
import scala.collection.mutable.IndexedBuffer

object AlignedGraphDissection:

  def rectangularDissection(
      graph: AlignedGraph,
  ): AlignedGraph =
    // (AlignedGraph, IndexedBuffer[(component: Set[NodeIndex], dockEdge: AlignedEdge, dockingNode: NodeIndex)]) =
    def addBoundingBox(edges: Seq[AlignedEdge], numComponents: Int): Seq[AlignedEdge]                                =
      val i         = edges.flatMap(e => Seq(e.from, e.to)).map(_.toInt).max + 1
      val dockEdges = Range(0, numComponents + 1)
        .map(index => AlignedEdge(NodeIndex(i + index), NodeIndex(i + index + 1), Direction.West))
      val newEdges  = dockEdges.++(
        Seq(
          AlignedEdge(NodeIndex(i + numComponents + 1), NodeIndex(i + numComponents + 2), Direction.North),
          AlignedEdge(NodeIndex(i + numComponents + 2), NodeIndex(i + numComponents + 3), Direction.East),
          AlignedEdge(NodeIndex(i + numComponents + 3), NodeIndex(i), Direction.South),
        ),
      )
      newEdges
    end addBoundingBox
    val dockEdgesWithNode: IndexedBuffer[(component: Set[NodeIndex], dockEdge: AlignedEdge, dockingNode: NodeIndex)] =
      IndexedBuffer.empty
    var faceEdgeCandidate                                                                                            = graph.getOneEdgePerFace()
    var allEdges                                                                                                     = graph.edges.toSet
    val connectedComponents                                                                                          = getConnectedComponents(graph)
    // TODO: add support for multiple connected components!
    val bb                                                                                                           = addBoundingBox(allEdges.toSeq, connectedComponents.size)
    allEdges.++=(bb)
    var currentGraph                                                                                                 = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
    var i                                                                                                            = 0
    var connectedComponentsCounter                                                                                   = 0
    while i < faceEdgeCandidate.size do
      var e = faceEdgeCandidate(i)
      if currentGraph.isOuterFace(e) then
        val edgesAfterCompaction = rectangularDissectFace(currentGraph, e.to, e)
        // update edges that were split
        faceEdgeCandidate = graph.updateEdgesAfterSplit(faceEdgeCandidate, edgesAfterCompaction)
        val allReplaced          = edgesAfterCompaction.replacedEdges.flatMap(e => graph.nodeAndReverseNode(e))
        allEdges = allEdges.++(edgesAfterCompaction.newEdges).toSet.--(allReplaced)
        val compactedGraph       = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
        e =
          if edgesAfterCompaction.newEdges.isEmpty then e
          else edgesAfterCompaction.newEdges.find(e => compactedGraph.isOuterFace(e)).get
        val outerFaceIter        = compactedGraph.traverseEdgesAlignedFace(e, false).toSeq
        val nodeToOuterFace      = getBottomRightCorner(compactedGraph, outerFaceIter)
        val edgeToBB             = AlignedEdge(nodeToOuterFace, bb(connectedComponentsCounter).to, Direction.South)
        dockEdgesWithNode
          .+=((getConnectedComponent(graph, nodeToOuterFace), bb(connectedComponentsCounter), nodeToOuterFace))
        connectedComponentsCounter = connectedComponentsCounter + 1;
        allEdges.+=(edgeToBB)
        val graphWithConnectedBB = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
        faceEdgeCandidate = faceEdgeCandidate.:+(edgeToBB)
        currentGraph = graphWithConnectedBB
      else
        val edgesAfterCompaction = rectangularDissectFace(currentGraph, e.to, e)
        // update edges that were split
        faceEdgeCandidate = graph.updateEdgesAfterSplit(faceEdgeCandidate, edgesAfterCompaction)
        val allReplaced          = edgesAfterCompaction.replacedEdges.flatMap(e => graph.nodeAndReverseNode(e))
        allEdges = allEdges.++(edgesAfterCompaction.newEdges).toSet.--(allReplaced)
        val compactedGraph       = AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
        currentGraph = compactedGraph
      end if
      i = i + 1
    end while
    // (AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph, dockEdgesWithNode)
    AlignedGraph.fromAlignedEdges(allEdges.toSeq).mkAlignedGraph
  end rectangularDissection

  def getConnectedComponent(graph: AlignedGraph, startNode: NodeIndex): Set[NodeIndex] =
    val adj = (v: NodeIndex) => graph.vertices(v.toInt).neighbors.map(_.toNode)
    bfs.traverse(adj, startNode).toSet
  end getConnectedComponent

  def getConnectedComponents(graph: AlignedGraph): Set[Set[NodeIndex]] =
    var uncheckedNodes                             = Range(0, graph.vertices.length).map(NodeIndex(_)).toSet
    val allComponents: mutable.Set[Set[NodeIndex]] = mutable.Set.empty
    while !uncheckedNodes.isEmpty do
      val startNode = uncheckedNodes.toSeq(0)
      val comp      = getConnectedComponent(graph, startNode)
      uncheckedNodes = uncheckedNodes.--(comp)
      allComponents.+=(comp)
    end while
    allComponents.toSet
  end getConnectedComponents

  def getCornerDirections(e1: AlignedEdge, e2: AlignedEdge): Option[(Direction, Direction)] =
    if isCorner(e1, e2) then Some(e1.direction, e2.direction) else Option.empty
  end getCornerDirections

  def isCorner(e1: AlignedEdge, e2: AlignedEdge): Boolean =
    e1.direction != e2.direction && e1.to == e2.from
  end isCorner

  def rectangularDissectFace(
      graph: AlignedGraph,
      startNode: NodeIndex,
      startEdge: AlignedEdge,
  ): (newEdges: Seq[AlignedEdge], replacedEdges: Seq[AlignedEdge]) =
    def checkForSequenceAtEnd(seq: IndexedSeq[AlignedEdge]): Boolean =
      def linksCornersToNumbers(a: AlignedEdge, b: AlignedEdge): Seq[Int] =
        if a.direction.turnCCW == b.direction then Seq(1)
        else if a.direction.reverse == b.direction then Seq(1, 1)
        else Seq(0)
      if seq.size < 4 then return false
      val s                                                               = seq.takeRight(4).sliding(2).flatMap(l => linksCornersToNumbers(l(0), l(1))).toSeq
      s.takeRight(3).equals(Seq(0, 0, 1)) || s.equals(Seq(0, 0, 1, 1))

    val res: mutable.Buffer[AlignedEdge]          = mutable.Buffer.empty
    val removedEdges: mutable.Buffer[AlignedEdge] = mutable.Buffer.empty

    if graph.vertices.size < 5 then return (graph.edges, Seq.empty)
    val recentEdges: mutable.IndexedBuffer[AlignedEdge] = mutable.IndexedBuffer.empty
    var lastDirectionLink                               = startEdge
    val faceIterator                                    = graph.traverseEdgesAlignedFace(startEdge, false, true)
    var stop                                            = false
    var startNodeCounter                                = 0
    while !stop do
      var nextEdge = faceIterator.next()
      if nextEdge.to == startNode then startNodeCounter = startNodeCounter + 1
      while nextEdge.direction.equals(lastDirectionLink.direction) && startNodeCounter < 4 do
        nextEdge = faceIterator.next()
        if nextEdge.to == startNode then startNodeCounter = startNodeCounter + 1
      end while
      lastDirectionLink = nextEdge
      recentEdges.addOne(nextEdge)
      if startNodeCounter > 3 then stop = true
      if checkForSequenceAtEnd(recentEdges.toIndexedSeq) then
        // replace sequence
        val sequence                                   = recentEdges.takeRight(4)
        val (newIndex, newEdgesFromSplit, allNewEdges) = graph.splitEdge(sequence(0), graph.vertices)
        val newEdge                                    = AlignedEdge(sequence(3).from, newIndex, sequence(1).direction.reverse)
        val recursiveGraph                             = AlignedGraph.fromAlignedEdges(allNewEdges.appended(newEdge)).mkAlignedGraph
        val rec                                        = rectangularDissectFace(recursiveGraph, newEdge.from, graph.getReverseEdge(newEdge))
        res.addAll((newEdgesFromSplit.appended(newEdge)).filter(e => !rec.replacedEdges.contains(e)))
        res.addAll(rec.newEdges)
        // res.--=(nodeAndReversNode(sequence(0)))
        if !(rec.newEdges.contains(sequence(0))) then removedEdges.addOne(sequence(0))
        res.--=(rec.replacedEdges.flatMap(r => graph.nodeAndReverseNode(r)))
        removedEdges.addAll(rec.replacedEdges.filter(e => !(newEdgesFromSplit.appended(newEdge)).contains(e)))
        stop = true
      end if
    end while
    (res.toSeq, removedEdges.toSeq)
  end rectangularDissectFace

  def getBottomRightCorner(graph: AlignedGraph, seq: Seq[AlignedEdge]): NodeIndex =
    // val allConseqPairOnOuterFace = seq.++(seq.take(1)).sliding(2).filter(l => isOuterFace(l(0)))
    val allConseqPairOnOuterFace = seq.sliding(2).filter(l => graph.isOuterFace(l(0)))
    val allBRCornersCandiates    = allConseqPairOnOuterFace.filter(l =>
      (l(0).direction == Direction.East && l(1).direction == Direction.North ||
        l(0).direction == Direction.East && l(1).direction == Direction.West ||
        l(0).direction == Direction.South && l(1).direction == Direction.North),
    ).toSeq
    val allBRCorners             = allBRCornersCandiates.map(l => l(1).from)
    /*val allBRCorners             = allBRCornersCandiates.map(l => l(1)).filter(x =>
      seq.find(e =>
        (e.direction == Direction.South && e.from == x.to) || (
          e.direction == Direction.North && e.to == x.to
        ),
      ).isEmpty,
    ).map(_.to) */
    if allBRCorners.isEmpty then sys.error("Outer face does not contain any bottom right corner!")
    allBRCorners(0)
  end getBottomRightCorner

end AlignedGraphDissection
