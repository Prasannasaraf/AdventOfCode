import org.scalatest.FunSuite

class Day7Test extends FunSuite {
  test("Day7 test Part1") {
    val input = Array(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31,
      1, 32, 31, 31, 4, 31, 99, 0, 0, 0)
    assert(Day7.calculate(input) == 65210)
  }

  test("Day7 test Part2-1") {
    val input = Array(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28,
      6, 99, 0, 0, 5)
    assert(Day7.feedbackCalculate(input) == 139629729)
  }

  test("Day7 test Part2-2") {
    val input = Array(3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5, 54,
      1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6,
      99, 0, 0, 0, 0, 10)
    assert(Day7.feedbackCalculate(input) == 18216)
  }
}
