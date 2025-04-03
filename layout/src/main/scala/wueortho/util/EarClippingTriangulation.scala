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
        val sameSide = triangle.permutations.map(l => orientationTest(l(0), l(1), l(2)) == orientationTest(l(0), l(1), p)).reduce(_||_)
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
        return orientationTest(_pos(prevNode.ref.toInt), _pos(nextNode.ref.toInt) ,_pos(_vertices(index).ref.toInt)) < 0

    /**
      * Tests, if the given node is part of a ear, by checking, if any other vertex is inside the spanned triangle
      *
      * @param node linkedNode to be tested
      * @return boolean, if ear
      */
    def testEar(index: Int):Boolean = 
        val prevNode = next(index)
        val nextNode = prev(index)
        val node = _vertices(index)
        val verticesInTriangle = _vertices.filter(linkedNode => linkedNode.ref != node.ref && linkedNode.ref != prevNode.ref && linkedNode.ref != nextNode.ref)
            .map(linkedNode => isInsideTriangle(_pos(node.ref.toInt), _pos(prevNode.ref.toInt), _pos(nextNode.ref.toInt) ,_pos(linkedNode.ref.toInt)) ).reduce(_||_)
        !verticesInTriangle

    /**
      * creates a triangulation for the given graph and vertex position
      *
      * @param graph the BasicGraph containing all edges
      * @param pos vertex positions
      * @return additionally edges for the triangulation
      */
    def triangulate(graph: BasicGraph, pos: Seq[Vec2D]): Seq[SimpleEdge] = 
        ???

    def updateVertexType(index: Int): Unit =
        if testEar(index) then _vertices(index).nodeType = NodeType.EAR
             else if testConvex(index) then _vertices(index).nodeType = NodeType.CONVEX
             else _vertices(index).nodeType = NodeType.REFLEX

    def trianglateFace(polygon: IndexedSeq[NodeIndex]): Seq[SimpleEdge] = 
        var result: Buffer[SimpleEdge] = Buffer.empty
        _vertices = polygon.map(index => Vertex(index, NodeType.REFLEX)).toBuffer
        for i <- 0 until _vertices.length do
            updateVertexType(i)
        end for
        while _vertices.find(link => link.nodeType == NodeType.EAR).isDefined do
            val index = _vertices.indexWhere(link => link.nodeType == NodeType.EAR)
            val prevI = prevIndex(index)
            val nextI = nextIndex(index)
            val prevVertex = prev(index)
            val nextVertex = next(index) 
            val currentElement = _vertices.remove(index)
            result.append(SimpleEdge(prevVertex.ref, nextVertex.ref))
            updateVertexType(prevI)
            updateVertexType(nextI)
        
        result.toSeq


end EarClippingTriangulation