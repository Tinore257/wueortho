package wueortho.io.grid

// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

import wueortho.data.*
import cats.instances.boolean


object GridGraph:
  case class GridGraphConfig(rows: Int, columns: Int, size: Double = 1.0, gap: Double = 1.0, diagonalEdges: Boolean = false)

  def mkGridGraph(config: GridGraphConfig): Either[String, BasicGraph] =
    import config.*

    val graph =
        val verticalEdges = for
        i <- 0 until rows - 1
        j <- 0 until columns
        yield SimpleEdge(NodeIndex(i * columns + j), NodeIndex((i + 1) * columns + j)) :: Nil


        val horizontalEdges = for
        i <- 0 until rows
        j <- 0 until columns - 1
        yield SimpleEdge(NodeIndex(i * columns + j), NodeIndex((i * columns + (j + 1)))) :: Nil

        
        val digoanalEdges = if diagonalEdges then for 
        i <- 0 until rows - 1
        j <- 0 until columns - 1
        yield SimpleEdge(NodeIndex(i * columns + j), NodeIndex(((i+1) * columns + (j + 1)))) :: 
            SimpleEdge(NodeIndex((i+1) * columns + j), NodeIndex((i * columns + (j + 1)))) :: Nil
        else Nil

        val allEdges = verticalEdges.appendedAll(horizontalEdges).appendedAll(digoanalEdges)
            .flatMap(identity)

        Graph.fromEdges(allEdges).mkBasicGraph

    /*val boxes = VertexBoxes:
        for
            i <- 0 until rows
            j <- 0 until columns
        yield Rect2D(Vec2D(j * (1 + gap) * size, -i * (1 + gap) * size), Vec2D(size / 2, size / 2))
    */

    Right(graph)

end GridGraph
