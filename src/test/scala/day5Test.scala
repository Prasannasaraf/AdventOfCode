import org.scalatest.FunSuite

class Day5Test extends FunSuite {
  test("Day5 Test") {
    val input = Array(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21,
      125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99) 
    assert(Day5.processInstruction(input.clone(), user_inputs = Iterator(5))._1 == 999)
    assert(Day5.processInstruction(input.clone(), user_inputs = Iterator(8))._1 == 1000)
    assert(Day5.processInstruction(input.clone(), user_inputs = Iterator(10))._1 == 1001)
  }
}
