package wueortho.util

package wueortho.util

import _root_.wueortho.data.*
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.compiletime.ops.double
import scala.collection.mutable.Buffer

object EarClippingTriangulation:

    enum NodeType derives CanEqual:
        case EAR, CONVEX, REFLEX



    case class Vertex(ref: NodeIndex, var nodeType: NodeType)

    private var _vertices: Buffer[Vertex] = Buffer.empty
    private var _pos: Seq[Vec2D] = Seq.empty


    def orientationTest(e1: Vec2D, e2: Vec2D, node: Vec2D): Double =
        val v = (e2.x2 - e1.x2) * (node.x1 - e2.x1) -
        (e2.x1 - e1.x1) * (node.x2 - e2.x2)
        if v == 0 then v else v.sign

    /**
      * returns true if the vertex position p is located inside triangle spanned by a, b and c
      * 
      *
      * @param a first vertex position of triangle
      * @param b second vertex position of triangle
      * @param c third vertex position of triangle
      * @param p vertex to test
      * @return boolean, if p is inside (a,b,c)
      */
    def isInsideTriangle(a: Vec2D, b: Vec2D, c: Vec2D, p: Vec2D): Boolean = 
        val triangle = Seq(a, b, c)
        val sameSide = triangle.permutations.map(l => orientationTest(l(0), l(1), l(2)) == orientationTest(l(0), l(1), p)).reduce(_&&_)
        sameSide

    def nextIndex(index: Int):Int = 
        val n = _vertices.length
        (index + 1) % n

    def prevIndex(index: Int):Int = 
        val n = _vertices.length
        (index-1+n)%n

    def next(index: Int): Vertex =
        _vertices(nextIndex(index))

    def prev(index: Int): Vertex =
        _vertices(prevIndex(index))

    /**
      * Tests, if the given node qualifies as convex using the vertices linked List
      *
      * @param node the linked node to test
      */
    def testConvex(index: Int):Boolean = 
        val nextNode = next(index) 
        val prevNode = prev(index)
        return orientationTest(_pos(prevNode.ref.toInt), _pos(nextNode.ref.toInt) ,_pos(_vertices(index).ref.toInt)) > 0

    /**
      * Tests, if the given node is part of a ear, by checking, if any other vertex is inside the spanned triangle
      *
      * @param node linkedNode to be tested
      * @return boolean, if ear
      */
    def testEar(index: Int):Boolean = 
        val prevNode = prev(index)
        val nextNode = next(index)
        val node = _vertices(index)
        val vertexInTrinagle = _vertices.filter(linkedNode => linkedNode.ref != node.ref && linkedNode.ref != prevNode.ref && linkedNode.ref != nextNode.ref)
            .map(linkedNode => isInsideTriangle(_pos(node.ref.toInt), _pos(prevNode.ref.toInt), _pos(nextNode.ref.toInt) ,_pos(linkedNode.ref.toInt)) )
        val verticesInTriangle = vertexInTrinagle.reduceOption(_||_)
        // TODO: how to deal with faces only containing a single or two vertices?
        verticesInTriangle.isEmpty || !verticesInTriangle.get

    /** gets all orthogonal (to direction) disjoint sets that are inside or part of a face consistinig of only aligned
      * edges
      */
    def getAllFaces(graph: BasicGraph, pos: Seq[Vec2D]): Seq[IndexedSeq[NodeIndex]] =

      def getAngle(a: NodeIndex, b: NodeIndex) =
        val delta = pos(b.toInt) - pos(a.toInt)
        Math.atan2(delta.x1, delta.x2)

      var faces = ArrayBuffer[IndexedSeq[NodeIndex]]().empty

      // adjacency list for each vertex containing only aligned edges
      val adjSortedByAngle = graph.vertices.zipWithIndex.map((v, i) =>
        (
          NodeIndex(i) ->
            v.neighbors.sortBy(link => getAngle(NodeIndex(i), link.toNode)).map(link => link.toNode),
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
          untraversedEdges.remove(untraversedEdges.indexOf(currentEdge))

          currentEdge = (currentEdge._2, toNodeAdjacencyList(nextEdgeIndex))

          currentEdge != startEdge
        do ()
        end while

        faces.addOne(newFace.toIndexedSeq)

      end while

      return faces.toSeq
    end getAllFaces
        
    /**
      * creates a triangulation for the given graph and vertex position
      *
      * @param graph the BasicGraph containing all edges
      * @param pos vertex positions
      * @return additionally edges for the triangulation
      */
    def triangulate(graph: BasicGraph, pos: Seq[Vec2D]): Any = 
        //val polygons = getAllFaces(graph, pos.toIndexedSeq)
        //_pos = pos
        _pos = IndexedSeq(Vec2D(3, 48),
          Vec2D(52, 8),
          Vec2D(99, 50),
          Vec2D(138, 25),
          Vec2D(175, 77),
          Vec2D(131, 72),
          Vec2D(111, 113), 
          Vec2D(72, 43),
          Vec2D(26, 55),
          Vec2D(29, 100)
          )
        val p1 = Range(0, 10).map(i => NodeIndex(i))
        val polygons = Seq(p1)
        //TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO 
        ////////////////////////////////////////////////////////
        for p <- polygons do
          val additionalEdges = triangulateFace(p)
          val _ = additionalEdges
        end for

    def updateVertexType(index: Int): Unit =
        if testConvex(index) && testEar(index) then _vertices(index).nodeType = NodeType.EAR
             else if testConvex(index) then _vertices(index).nodeType = NodeType.CONVEX
             else _vertices(index).nodeType = NodeType.REFLEX

    def triangulateFace(polygon: IndexedSeq[NodeIndex]): Seq[SimpleEdge] = 
        var result: Buffer[SimpleEdge] = Buffer.empty
        _vertices = polygon.map(index => Vertex(index, NodeType.REFLEX)).toBuffer
        for i <- 0 until _vertices.length do
            updateVertexType(i)
        end for
        while _vertices.size > 3 && _vertices.find(link => link.nodeType == NodeType.EAR).isDefined do
            val index = _vertices.indexWhere(link => link.nodeType == NodeType.EAR)
            val currentElement = _vertices.remove(index)
            val prevI = prevIndex(index)
            val nextI = nextIndex(prevI)
            val prevVertex = _vertices(prevI)
            val nextVertex = _vertices(nextI) 
            result.append(SimpleEdge(prevVertex.ref, nextVertex.ref))
            updateVertexType(prevI)
            updateVertexType(nextI)
        
        result.toSeq


end EarClippingTriangulation