import scala.io.Source

object Day8 {
  def main(args: Array[String]) {
    val input = Source.fromResource("day8").getLines.toSeq.mkString.toCharArray()
    val height = 6
    val width = 25
    println(calculate(input, height, width))
    printLayer(calculatePart2(input, height, width), width)
  }

  def calculate(input: Array[Char], height: Int, width: Int): Int = {
    val layerSize = height * width
    val allLayer = input.grouped(layerSize).map(group => group.count(_ == '0') -> group).toMap
    val actualLayer = allLayer(allLayer.keys.min)
    actualLayer.count(_ == '1') * actualLayer.count(_ == '2')
  }

  def calculatePart2(input: Array[Char], height: Int, width: Int): Array[Char] = {
    val layerSize = height * width
    val allLayer = input.grouped(layerSize)
    allLayer.foldLeft(Array.fill(layerSize)('2')) {
      case (acc, layer) => acc zip layer map layerMerger
    }
  }

  def layerMerger(tuple: (Char, Char)): Char = {
    tuple._1 match {
      case '2' => tuple._2
      case _   => tuple._1
    }
  }

  def printLayer(layer: Array[Char], width: Int) = {
    layer.map(x => if (x == '0') ' ' else x).grouped(width).foreach(string => println(string.mkString))
  }
}
