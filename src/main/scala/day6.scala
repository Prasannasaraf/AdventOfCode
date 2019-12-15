import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import Logger.logInfo

case class Node(data: String, childs: ArrayBuffer[Node]) {
  def update(node: Node): Unit = {
    childs += node
    logInfo(childs, data)
  }

  def isLeaf = childs.isEmpty
}

object Day6 {

  val separator = ')'

  def totalIndirect(root: Node, level: Int): Int = {
    if (root.isLeaf) return level
    root.childs
      .map(child => {
        totalIndirect(child, level + 1)
      })
      .sum + level
  }

  def main(args: Array[String]): Unit = {
    val inputFile = "day6"
    val input = Source.fromResource(inputFile).getLines.toSeq
    println(orbitsSwapCount(input))
  }

  def childNodesCreator(input: Seq[String]): Map[String, Node] = {
    input.map(keyValue => {
      val value = keyValue.split(separator)(1)
      value -> Node(value, ArrayBuffer.empty)
    }) :+ ("COM" -> Node("COM", ArrayBuffer.empty)) toMap
  }

  def treeCreator(input: Seq[String], childNodes: Map[String, Node]) = {
    input.foreach(
      keyValue => {
        val key = keyValue.split(separator)(0)
        val value = keyValue.split(separator)(1)
        childNodes(key).update(childNodes(value))
      }
    )
    logInfo("size ", input.size)
    childNodes("COM")
  }

  def orbitCount(input: Seq[String]): Int = {
    val childNodes = childNodesCreator(input)
    val root = treeCreator(input, childNodes)
    totalIndirect(root, 0)
  }

  def orbitsSwapCount(input: Seq[String]): Int = {
    val childNodes = childNodesCreator(input)
    val root = treeCreator(input, childNodes)
    findChildAlt(childNodes("YOU"), childNodes("SAN"), childNodes("COM"))
  }

  def buildGraph(node: Node, root1: Node): Seq[Node] = {
    def buildGraphInternal(root: Node, acc: Seq[Node]): Seq[Node] = {
      if (node == root) return acc :+ root
      if (root.isLeaf) return Seq.empty
      root.childs.flatMap {
        case child => buildGraphInternal(child, acc :+ root)
      }
    }
    buildGraphInternal(root1, Seq.empty)
  }

  def findCommon(graph1: Seq[Node], graph2: Seq[Node]): Node = {
    Logger.logInfo(graph1, graph2)
    graph1.zip(graph2).foldLeft(graph1.head) {
      case (acc, (node1, node2)) if (node1 == node2) => node1
      case (acc, _)                                  => return acc
    }
    graph1.last
  }

  def distanceBetween(commonNode1: Node, from: Node): Int = {
    def internalDistanceBetween(commonNode: Node, acc: Int): Int = {
      if (commonNode == from) return acc
      if (commonNode.isLeaf) return 0
      commonNode.childs.map {
        case child => internalDistanceBetween(child, acc + 1)
      }.sum
    }
    internalDistanceBetween(commonNode1, -1)
  }

  def findChildAlt(node1: Node, node2: Node, root: Node): Int = {
    val graph1 = buildGraph(node1, root)
    val graph2 = buildGraph(node2, root)
    val commonRoot = findCommon(graph1, graph2)
    val orbits1 = distanceBetween(commonRoot, node1)
    val orbits2 = distanceBetween(commonRoot, node2)
    orbits1 + orbits2
  }
}
