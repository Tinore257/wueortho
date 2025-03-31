// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util.mutable

import scala.collection.mutable
import scala.collection.mutable.Buffer
import wueortho.data.*
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.ops.double

class DCEL():

  case class EdgeIndex(id: Int):
    def toInt(): Int =
      id

  case class FaceIndex(id: Int):
    def toInt(): Int =
      id

  case class DCELNode(id: NodeIndex, edge: Option[EdgeIndex])
  case class DCELEdge(
      from: NodeIndex,
      to: NodeIndex,
      prev: EdgeIndex,
      next: EdgeIndex,
      twin_ptr: EdgeIndex,
      face_ptr: FaceIndex,
  )
  case class DCELFace(node: Option[EdgeIndex])

  private var _nodeArray: Buffer[DCELNode] = Buffer.empty
  private var _faceArray: Buffer[DCELFace] = Buffer.empty
  private var _edgeArray: Buffer[DCELEdge] = Buffer.empty
  private var _pos: Buffer[Vec2D]          = Buffer.empty

  def this(graph: WeightedGraph, pos: IndexedSeq[Vec2D]) =
    this()

    /** gets all orthogonal (to direction) disjoint sets that are inside or part of a face consistinig of only aligned
      * edges
      */
    def getAllFaces(graph: DiGraph): Seq[IndexedSeq[NodeIndex]] =

      def getAngle(a: NodeIndex, b: NodeIndex) =
        val delta = pos(b.toInt) - pos(a.toInt)
        Math.atan2(delta.x1, delta.x2)

      var faces = ArrayBuffer[IndexedSeq[NodeIndex]]().empty

      // adjacency list for each vertex containing only aligned edges
      val adjSortedByAngle = graph.vertices.zipWithIndex.map((v, i) =>
        (
          NodeIndex(i) ->
            v.neighbors.sortBy(link => getAngle(NodeIndex(i), link)),
          ),
      )

      val untraversedEdges = adjSortedByAngle.flatMap((from, neighbors) => neighbors.map(n => (from, n))).toBuffer

      while untraversedEdges.size > 0 do
        val startEdge = untraversedEdges(0)

        var currentEdge = startEdge

        val newFace: Buffer[NodeIndex] = Buffer().empty

        while
          val toNodeAdjacencyList = adjSortedByAngle(currentEdge._2.toInt)._2

          val nextEdgeIndex = (toNodeAdjacencyList.indexOf(currentEdge._1) + 1) % toNodeAdjacencyList.size

          newFace.append(currentEdge._1)
          // newFace.append(currentEdge._2)

          untraversedEdges.remove(untraversedEdges.indexOf(currentEdge))

          currentEdge = (currentEdge._2, toNodeAdjacencyList(nextEdgeIndex))

          currentEdge != startEdge
        do ()
        end while

        faces.addOne(newFace.toIndexedSeq)

      end while

      return faces.toSeq
    end getAllFaces

    // first create all nodes without eny edges
    val allNodes = graph.vertices.zipWithIndex.map((v, i) => DCELNode(NodeIndex(i), None))

    val diGraph = Graph.fromEdges(
      graph.edges.flatMap(e =>
        Seq(
          SimpleEdge(
            e.from,
            e.to,
          ),
          SimpleEdge(e.to, e.from),
        ),
      ),
      graph.numberOfVertices,
    ).mkDiGraph

    // create then all edges
    val allFaces = getAllFaces(diGraph)

    val allHalfEdes = diGraph.edges.zipWithIndex

    val halfEdgesGroupedByOrigin = allHalfEdes.groupBy((e, _) => e.from)

    // id, edge, twin
    val allHalfEdgesWithTwin = allHalfEdes.map(e =>
      (
        e._2,
        e._1,
        halfEdgesGroupedByOrigin.getOrElse(e._1.from, sys.error("node was not found while creating half-edges"))
          .filter(twin => twin._1.to == e._1.from).last._2,
      ),
    )

    case class edgePN(from: NodeIndex, to: NodeIndex, next: NodeIndex, prev: NodeIndex)

    val edgesWithNext = allFaces.flatMap(face => {
      var edgesWithPrevAndNext = mutable.ArrayBuffer[edgePN]().empty
      val s                    = face.size
      for i <- 0 until s do
        edgesWithPrevAndNext
          .append(edgePN(face((i.toInt + 1) % s), face((i.toInt + 2) % s), face((i.toInt + 3) % s), face(i.toInt)))
      end for
      edgesWithPrevAndNext
    })

  end this
  // create NodeArray
  // create NodeArray

  def addNode(id: NodeIndex, pos: Vec2D) =
    ???

  def removeNode(id: NodeIndex) =
    ???

end DCEL
