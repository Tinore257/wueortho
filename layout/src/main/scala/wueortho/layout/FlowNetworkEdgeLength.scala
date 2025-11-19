package wueortho.layout

import wueortho.data.*
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.alg.flow.mincost.CapacityScalingMinimumCostFlow
import org.jgrapht.alg.flow.mincost.MinimumCostFlowProblem.MinimumCostFlowProblemImpl
import scala.collection.mutable
import scala.collection.mutable.IndexedBuffer
import scala.jdk.CollectionConverters.*
import wueortho.layout.AlignedGraphDissection

object FlowNetworkEdgeLength:

  case class FaceWithEdgesPerDirection(
      var topEdges: Seq[AlignedEdge],
      var leftEdges: Seq[AlignedEdge],
      var bottomEdges: Seq[AlignedEdge],
      var rightEdges: Seq[AlignedEdge],
  ):
    def update(edges: Seq[AlignedEdge], dir: Direction) =
      dir match
        case Direction.North => topEdges = edges
        case Direction.East  => rightEdges = edges
        case Direction.South => bottomEdges = edges
        case Direction.West  => leftEdges = edges

    def getInDirection(dir: Direction): Seq[AlignedEdge] =
      dir match
        case Direction.North => topEdges
        case Direction.East  => rightEdges
        case Direction.South => bottomEdges
        case Direction.West  => leftEdges
  end FaceWithEdgesPerDirection

  object FaceWithEdgesPerDirection:
    def empty: FaceWithEdgesPerDirection =
      FaceWithEdgesPerDirection(Seq.empty, Seq.empty, Seq.empty, Seq.empty)

  def getMapEdgeToAdjacentFace(
      graph: AlignedGraph,
      facesReps: IndexedBuffer[AlignedEdge],
  ): Map[AlignedEdge, IndexedBuffer[Int]] =
    /*def isSameOrReverseEdge(e1: AlignedEdge, e2: AlignedEdge): Boolean =
                def isSameEdge(e1: AlignedEdge, e2: AlignedEdge): Boolean =
                e1.from == e2.from && e1.to == e2.to
                isSameEdge(e1, e2) || isSameEdge(e1, getReverseEdge(e2))*/
    def edgeSortedFromTo(e: AlignedEdge): AlignedEdge =
      if (e.from.toInt < e.to.toInt) then e else graph.getReverseEdge(e)
    val allEdges                                      = facesReps.zipWithIndex.flatMap((rep, i) =>
      graph.traverseEdgesAlignedFace(rep, false).toSeq.dropRight(1).map(edgeSortedFromTo(_)).map(l => (i, l)),
    )
    val groupedEdges                                  = allEdges.groupBy((_, edge) => edge)
    val edgeToFaceMap                                 = groupedEdges.map((k, v) => (k, v.map((face, _) => face)))
    val bothEdgesToFaceMap                            = edgeToFaceMap.flatMap((k, v) => Seq((k, v), (graph.getReverseEdge(k), v)))
    bothEdgesToFaceMap
  end getMapEdgeToAdjacentFace

  def createFlowNetwork(
      graph: AlignedGraph,
      dir: Direction = Direction.North,
  ): (DirectedMultigraph[Int, DefaultEdge], Map[DefaultEdge, AlignedEdge]) =
    // create a node for each (inner) face
    val g = DirectedMultigraph[Int, DefaultEdge](classOf[DefaultEdge]);

    val faceReps = graph.getOneEdgePerFace()

    // val innerFaceReps = faceReps.filter(f => !isOuterFace(f));
    val outerFaceIndex = faceReps.zipWithIndex.filter((f, _) => graph.isOuterFace(f)).map((_, i) => i)

    if outerFaceIndex.isEmpty then sys.error("No outer face found!")

    for i <- 0 to faceReps.length do g.addVertex(i)
    end for

    val s = g.vertexSet().size()
    g.addVertex(s)

    val t = outerFaceIndex(0)

    // get map from edge to faces
    val edgeToFaceMap = getMapEdgeToAdjacentFace(graph, faceReps)

    val faceToEdgesInDir = faceReps
      .map(_ => FaceWithEdgesPerDirection.empty) // mutable.IndexedBuffer[FaceWithEdgesPerDirection]().empty

    val flowEdgeToEdgeMap: mutable.Map[DefaultEdge, AlignedEdge] = mutable.Map.empty;

    var allFlowEdgesAndOriginal: Seq[(DefaultEdge, AlignedEdge)] = Seq.empty;

    // set for each face for each direction the bounding edges
    for i <- 0 to faceReps.length - 1 do
      for dir <- Direction.values.toSeq do
        val edgesInDirection = graph
          .getAllEdgesOfFaceInDirection(i, faceReps.toIndexedSeq, dir, graph.isOuterFace(faceReps(i)))
        val currentFace      = faceToEdgesInDir(i)
        currentFace.update(edgesInDirection, dir)
      end for
    end for
    // add directed edges to graph
    for i <- 0 to faceReps.length - 1 do
      if !graph.isOuterFace(faceReps(i)) then
        val edgesInDirection                                                         = faceToEdgesInDir(i).getInDirection(dir)
        val allNeighboringFaces: Seq[(adjFace: Int, correspondingEdge: AlignedEdge)] = edgesInDirection
          .flatMap(e => (edgeToFaceMap.get(e).getOrElse(Seq.empty).map((_, e)))).filter((f, _) => f != i)
        // TODO: Es können eine oder mehrer Facetten benachbart sein. Falls nur eine Facette über
        // mehere Kanten adjazent sind, wird der Arc nur einmal hinzugefügt, für alle anderen gibt
        // das hinzufügen "null" zurück, weil die Kante bereits existiert => vorher groupBy adjazent Face,
        // dann den Arc hinzufügen und für alle gruppierten Kanten, den Arc setzen
        val newFlowEdges                                                             = allNeighboringFaces
          .map(faceWithEdge => (g.addEdge(i, faceWithEdge.adjFace), faceWithEdge.correspondingEdge)).toSeq
        // val newFlowEdges                                                             = newArcWithAllEdges
        //  .flatMap((newArc, faceWithEdge) => faceWithEdge.map(e => (newArc, e.correspondingEdge)).toSeq)
        allFlowEdgesAndOriginal.++=(newFlowEdges);
    end for
    // connect s to the remaining graph
    val outerFace = faceToEdgesInDir(outerFaceIndex(0))
    val edgesInDirection                                                         = outerFace.getInDirection(dir.reverse)
    val allNeighboringFaces: Seq[(adjFace: Int, correspondingEdge: AlignedEdge)] = edgesInDirection
      .flatMap(e => (edgeToFaceMap.get(e).getOrElse(Seq.empty).map((_, e)))).filter((f, _) => f != s && f != t)
    val newFlowEdges                                                             = allNeighboringFaces
      .map(faceWithEdge => (g.addEdge(s, faceWithEdge.adjFace), faceWithEdge.correspondingEdge))
    allFlowEdgesAndOriginal.++=(newFlowEdges);
    // flow back
    g.addEdge(t, s)

    val allEdges = g.edgeSet.toArray().toSeq

    val mapFlowArcToEdge = allFlowEdgesAndOriginal.toMap

    (g, mapFlowArcToEdge)

  end createFlowNetwork

  def solveFlowNetwork(network: org.jgrapht.Graph[Int, DefaultEdge]): Seq[(DefaultEdge, Double)] =

    val nodeDemand: java.util.function.Function[Int, Integer] = (_: Int) => 0

    val minArcCapacityFunc: java.util.function.Function[DefaultEdge, Integer] = (_: DefaultEdge) => 1

    val maxArcCapacityFunc: java.util.function.Function[DefaultEdge, Integer] = (_: DefaultEdge) =>
      CapacityScalingMinimumCostFlow.CAP_INF

    // val costFunc: java.util.function.Function[DefaultEdge, Double] = (_: DefaultEdge) => 1.0

    val problemInstance =
      MinimumCostFlowProblemImpl[Int, DefaultEdge](
        network,
        nodeDemand,
        maxArcCapacityFunc,
        minArcCapacityFunc,
        // costFunc,
      )

    val minCostFlow = CapacityScalingMinimumCostFlow[Int, DefaultEdge]()

    val flow = minCostFlow.getMinimumCostFlow(problemInstance)

    val totalWidth = flow.getFlow

    val keyMap = flow.getFlowMap;

    keyMap.entrySet().asScala.toSeq.map(entry => (entry.getKey(), entry.getValue()))

  end solveFlowNetwork

  def positionsFromEdgeLength(graph: AlignedGraph): VertexLayout =

    val accumulatedEdgeLengths = determineEdgeLength(graph);

    val graphWithEdgeLength = AlignedWithLengthGraph.fromAlignedWithLengthEdges(accumulatedEdgeLengths)
      .mkAlignedWithLengthGraph;

    val nodePositions = graphWithEdgeLength.getPositions();

    VertexLayout(nodePositions.toIndexedSeq)
  end positionsFromEdgeLength

  def determineEdgeLength(graph: AlignedGraph): Seq[AlignedWithLengthEdge] =
    val dissectedGraph = AlignedGraphDissection.rectangularDissection(graph);

    val (verticalFlowNetwork, verticalArcToEdgeMap)     = createFlowNetwork(dissectedGraph, Direction.North)
    val (horizontalFlowNetwork, horizontalArcToEdgeMap) = createFlowNetwork(dissectedGraph, Direction.East)

    val horizontalArcLengths = solveFlowNetwork(verticalFlowNetwork)
    val verticalArcLegnths   = solveFlowNetwork(horizontalFlowNetwork)

    val horEdgeLengths = horizontalArcLengths.filter((e, _) => verticalArcToEdgeMap.contains(e))
      .map((e, len) => AlignedWithLengthEdge().fromAlignedEdge(verticalArcToEdgeMap(e), len.toInt))

    val vertEdgeLengths = verticalArcLegnths.filter((e, _) => horizontalArcToEdgeMap.contains(e))
      .map((e, len) => AlignedWithLengthEdge().fromAlignedEdge(horizontalArcToEdgeMap(e), len.toInt))

    val dissectedEdgesWithLength = horEdgeLengths.++(vertEdgeLengths)

    val dissectedGraphWithLength = AlignedWithLengthGraph.fromAlignedWithLengthEdges(dissectedEdgesWithLength)
      .mkAlignedWithLengthGraph

    // val originalEdgesWithLength = dissectedGraphWithLength
    // .filter(e => e._1.from.toInt < this.vertices.length && e._1.to.toInt <
    // this.vertices.length)

    val originalEdgesWithLength = graph.edges.map(e => dissectedGraphWithLength.getDissectedEdges(e))

    val accumulatedEdgeLengths = originalEdgesWithLength.map(_.map(_.length).reduce(_ + _)).zip(graph.edges)
      .map((length, edge) => AlignedWithLengthEdge(edge.from, edge.to, edge.direction, length))

    accumulatedEdgeLengths
  end determineEdgeLength

end FlowNetworkEdgeLength
