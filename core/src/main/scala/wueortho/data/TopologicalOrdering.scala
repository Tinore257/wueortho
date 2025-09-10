package wueortho.data
import scala.collection.mutable
import scala.collection.mutable.IndexedBuffer

class TopologicalOrdering():
  val lists: mutable.IndexedBuffer[mutable.IndexedBuffer[NodeIndex]] = mutable.IndexedBuffer()

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

  def findListIndex(node: NodeIndex): Option[Int] =
    lists.zipWithIndex.find((l, _) => l.contains(node)).map((_, i) => i)
  end findListIndex

  def addAfter(node: NodeIndex, newNode: NodeIndex): mutable.IndexedSeq[NodeIndex] =
    val index = findListIndex(node).getOrElse(0)

    val newList = mutable.IndexedBuffer(newNode)
    lists.insert(index + 1, newList)
    return newList
  end addAfter

  def addBefore(node: NodeIndex, newNode: NodeIndex): mutable.IndexedSeq[NodeIndex] =
    val index   = findListIndex(node).getOrElse(0)
    val newList = mutable.IndexedBuffer(newNode)
    lists.insert(index, newList)
    return newList
  end addBefore

  def deleteNode(node: NodeIndex): Unit =
    val _ = lists.zipWithIndex.find((l, _) => l.contains(node)) match
      case Some((list, i)) => if list.size == 1 then lists.remove(i) else list.remove(list.indexOf(node))
      case None            => ()
  end deleteNode

  /** @param node
    *   the list which node is inside will be split
    * @param f
    *   the rule to split the list. If f is true for a element, it will be placed inside the first list, otherwise in
    *   the second
    */
  def splitList(
      node: NodeIndex,
      f: NodeIndex => Boolean,
  ): (before: mutable.Seq[NodeIndex], after: mutable.Seq[NodeIndex]) =
    val indexOpt        = findListIndex(node)
    if indexOpt.isEmpty then return (mutable.Seq.empty, mutable.Seq.empty)
    val index           = indexOpt.get
    val originalList    = lists(index)
    val (before, after) = originalList.partition(f)
    lists.update(index, before)
    lists.insert(index + 1, after)
    (before, after)
  end splitList

  def nextList(node: NodeIndex): Option[NodeIndex] =
    findListIndex(node) match
      case Some(index) => if index + 1 < lists.length then Some(lists(index + 1)(0)) else None
      case None        => None
  end nextList

  def prevList(node: NodeIndex): Option[NodeIndex] =
    findListIndex(node) match
      case Some(index) => if index > 0 then Some(lists(index - 1)(0)) else None
      case None        => None
  end prevList

  def getRange(startNode: NodeIndex, endNodeEx: NodeIndex): Seq[mutable.Seq[NodeIndex]] =
    val startIndex = findListIndex(startNode)
    if startIndex.isEmpty then return Seq.empty
    val range      = lists.drop(startIndex.get).takeWhile(!_.contains(endNodeEx))
    range.toSeq
  end getRange

  def createFromAlignedGraph(graph: AlignedGraph): TopologicalOrdering =
    def hasNeighborInDirection(node: NodeIndex, dir: Direction): Boolean         =
      graph.vertices(node.toInt).neighbors.exists(_.direction == dir)
    def getNextNeighborInDir(node: NodeIndex, dir: Direction): Option[NodeIndex] =
      graph.vertices(node.toInt).neighbors.find(_.direction == dir) match
        case Some(link) => Some(link.toNode)
        case None       => None

    var localLists: mutable.IndexedBuffer[mutable.IndexedBuffer[NodeIndex]] = mutable.IndexedBuffer.empty

    val remainingNodes = mutable.IndexedBuffer(graph.vertices.zipWithIndex.map((_, i) => NodeIndex(i))*)
    if remainingNodes.size == 0 then return this
    while remainingNodes.size > 0 do
      val southNeighbor                             = graph.vertices(remainingNodes(0).toInt).neighbors.filter(l => l.direction == Direction.South)
      var currentNode: Option[NodeIndex]            =
        if southNeighbor.isEmpty then Some(remainingNodes(0)) else Some(southNeighbor(0).toNode)
      val newList: mutable.IndexedBuffer[NodeIndex] = mutable.IndexedBuffer.empty
      while hasNeighborInDirection(currentNode.get, Direction.South) && currentNode.get != remainingNodes(0) do
        currentNode = getNextNeighborInDir(currentNode.get, Direction.South)
      end while
      while currentNode.isDefined && remainingNodes.contains(currentNode.get) do
        newList.addOne(currentNode.get)
        val index = remainingNodes.indexOf(currentNode.get)
        val _     = remainingNodes.remove(index)
        currentNode = getNextNeighborInDir(currentNode.get, Direction.North)
      end while
      localLists.addOne(newList)
    end while

    lists.addAll(localLists)
    // sort all lists
    // TODO: Use disjointSets
    // List with indices from lists to lists with all links in direction east
    val vertices      = graph.vertices.zipWithIndex
    val nodeLinkPair  = vertices.flatMap((l, i) => l.neighbors.map(l => (NodeIndex(i), l)))
      .filter((_, v) => v.direction == Direction.East)
    val listLinkPair  = nodeLinkPair.map((id, link) => (findListIndex(id).get, findListIndex(link.toNode)))
    val listNeighbors = listLinkPair.map((listIndex, toNode) => (listIndex, toNode.get)).distinct
      .groupBy((listIndex, _) => listIndex)

    val neighborsFunction: NodeIndex => Seq[NodeIndex] = x =>
      // getOrElse oder match, weil die Map ein Option zuerückgibt
      listNeighbors.getOrElse(x.toInt, Seq.empty).map((_, neigbor) => NodeIndex(neigbor))
    val sortedLists                                    = topologicalSort(
      lists.zipWithIndex.map((_, i) => NodeIndex(i)).toSeq,
      neighborsFunction,
    )
    localLists = localLists.zip(sortedLists.map(_.toInt)).sortBy((_, ordering) => ordering).map((lists, _) => lists)
      .reverse
    this
  end createFromAlignedGraph

end TopologicalOrdering
