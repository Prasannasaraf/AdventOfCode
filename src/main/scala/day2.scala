import scala.util.control.Breaks._

object Day2 {
  val testInput = Array(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 13,
    1, 19, 1, 19, 10, 23, 2, 10, 23, 27, 1, 27, 6, 31, 1, 13, 31, 35, 1, 13, 35,
    39, 1, 39, 10, 43, 2, 43, 13, 47, 1, 47, 9, 51, 2, 51, 13, 55, 1, 5, 55, 59,
    2, 59, 9, 63, 1, 13, 63, 67, 2, 13, 67, 71, 1, 71, 5, 75, 2, 75, 13, 79, 1,
    79, 6, 83, 1, 83, 5, 87, 2, 87, 6, 91, 1, 5, 91, 95, 1, 95, 13, 99, 2, 99,
    6, 103, 1, 5, 103, 107, 1, 107, 9, 111, 2, 6, 111, 115, 1, 5, 115, 119, 1,
    119, 2, 123, 1, 6, 123, 0, 99, 2, 14, 0, 0)

  def main(args: Array[String])= {
    println(s"part1: ${testInput(0)}")
    println(part2)
  }

  def part2: String = {
    for (noun <- 1 to 99) {
        for (verb <- 1 to 99) {
          val clonedTestInput = testInput.clone()
          clonedTestInput(1) = noun
          clonedTestInput(2) = verb
          Performance.time { processInstruction(0, clonedTestInput, 0) }
          if (clonedTestInput(0) == 19690720) {
            return s"Done: noun: $noun; verb: $verb Answer: ${(100 * noun) + verb}"
          }
        }
      }
      return ""
  }

  def processInstruction(index: Int, input: Array[Int], depth: Int): Unit = {
    val operation = input(index)
    // if (depth % 100 == 0) println(s"Depth : $depth")
    operation match {
      case 99 => return
      case x if (x == 1 || x == 2) => {
        val a = input(input(index + 1))
        val b = input(input(index + 2))
        val outPos = input(index + 3)
        input(outPos) = x match {
          case 1 => a + b
          case 2 => a * b
        }
        processInstruction(index + 4, input, depth + 1)
      }
      case _ => println("found something else")
    }
  }
}
