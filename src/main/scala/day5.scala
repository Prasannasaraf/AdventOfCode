case class OPCode(paramModes: Seq[Int], operation: Int) {
  def retreive(input: Seq[Int], index: Int, param: Int): Int = {
    paramModes(param - 1) match {
      case 0 => input(input(index + param))
      case 1 => input(index + param)
    }
  }
}

object OPCode {
  def apply(opcode: Int): OPCode = {
    val s = opcode.toString
    val prefix = "0000"
    val pfxOPCode = (prefix.take(5 - s.length) + s).map(char => char.toString.toInt)
    OPCode(Seq(pfxOPCode(2), pfxOPCode(1), pfxOPCode(0)), pfxOPCode(3) * 10 + pfxOPCode(4))
  }
}

object Day5 {
  def main(args: Array[String]) = {
    val input =
      "3,225,1,225,6,6,1100,1,238,225,104,0,1001,152,55,224,1001,224,-68,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1101,62,41,225,1101,83,71,225,102,59,147,224,101,-944,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,2,40,139,224,1001,224,-3905,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1101,6,94,224,101,-100,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,75,30,225,1102,70,44,224,101,-3080,224,224,4,224,1002,223,8,223,1001,224,4,224,1,223,224,223,1101,55,20,225,1102,55,16,225,1102,13,94,225,1102,16,55,225,1102,13,13,225,1,109,143,224,101,-88,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1002,136,57,224,101,-1140,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,101,76,35,224,1001,224,-138,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,344,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,389,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,404,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,419,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,434,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,449,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,464,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,509,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,524,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,539,101,1,223,223,107,226,226,224,1002,223,2,223,1006,224,554,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,599,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,614,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,629,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,644,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
        .split(",")
        .map(_.toInt)
    val user_input = 5
    val ans = processInstruction(0, input, 0, user_input, 0)
    println(ans)
  }

  def processInstruction(index: Int, input: Array[Int], depth: Int, user_input: Int, ans: Int): Int = {
    val opcode = OPCode(input(index))
    println(opcode)
    opcode.operation match {
      case 99 => {
        println("program halting")
        return ans
      }
      case x if (x == 1 || x == 2) => {
        val a = opcode.retreive(input, index, 1)
        val b = opcode.retreive(input, index, 2)
        val outPos = input(index + 3)
        input(outPos) = x match {
          case 1 => a + b
          case 2 => a * b
        }
        return processInstruction(index + 4, input, depth + 1, user_input, ans)
      }
      case x if (x == 3 || x == 4) => {
        val newAns = x match {
          case 3 =>
            val outpos = input(index + 1)
            input(outpos) = user_input
            ans
          case 4 => opcode.retreive(input, index, 1)
        }
        return processInstruction(index + 2, input, depth + 1, user_input, newAns)
      }
      case x if (x == 5 || x == 6) => {
        val instructionPointer = (opcode.retreive(input, index, 1), x) match {
          case (0, 5)             => index + 3
          case (x, 6) if (x != 0) => index + 3
          case (x, 5) if (x != 0) => opcode.retreive(input, index, 2)
          case (0, 6)             => opcode.retreive(input, index, 2)
        }
        return processInstruction(instructionPointer, input, depth + 1, user_input, ans)
      }
      case x if (x == 7 || x == 8) => {
        val param1 = opcode.retreive(input, index, 1)
        val param2 = opcode.retreive(input, index, 2)
        val param3 = input(index+3)
        (x, (param1 < param2), (param1 == param2)) match {
          case (7, true, _)  => input(param3) = 1
          case (7, false, _) => input(param3) = 0
          case (8, _, true)  => input(param3) = 1
          case (8, _, false) => input(param3) = 0
          case (_, _, _)     => println("Compiler doesn't understand hence the default")
        }
        return processInstruction(index + 4, input, depth + 1, user_input, ans)
      }
      case x =>
        println(s"found something else $opcode")
        return 0
    }
  }

}
