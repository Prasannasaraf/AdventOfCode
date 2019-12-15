object Day7 {
  def main(args: Array[String]) {
    val input: Array[Int] = Array(3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 38, 55, 68, 93, 118, 199, 280, 361, 442,
      99999, 3, 9, 1002, 9, 2, 9, 101, 5, 9, 9, 102, 4, 9, 9, 4, 9, 99, 3, 9, 101, 3, 9, 9, 1002, 9, 3, 9, 1001, 9, 4,
      9, 4, 9, 99, 3, 9, 101, 4, 9, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 101, 4, 9, 9, 102, 2, 9, 9, 1001, 9,
      4, 9, 102, 4, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 1001, 9, 2, 9, 1002, 9, 5, 9, 1001, 9, 2, 9, 1002, 9, 4, 9, 4,
      9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3,
      9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002,
      9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9,
      2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4,
      9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9,
      3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9,
      1001, 9, 1, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9,
      2, 9, 4, 9, 99, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9,
      9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4,
      9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9,
      3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9,
      1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99)

    println("part1", calculate(input))
    println("part2", feedbackCalculate(input))
  }

  def calculate(input: Array[Int]): Int = {
    val phaseSettings = Array(0, 1, 2, 3, 4).permutations.toSeq
    phaseSettings.map { phaseSetting =>
      phaseSetting.foldLeft(0) {
        case (acc, ampPhaseSettings) => (Day5.processInstruction(input.clone(), 0, Iterator(ampPhaseSettings, acc)))._1
      }
    }.max
  }

  def feedbackCalculate(input: Array[Int]): Int = {
    val phaseSettings = Array(5, 6, 7, 8, 9).permutations.toArray
    phaseSettings.map { phaseSetting =>
      val inputs: Array[Array[Int]] = phaseSettings.toArray.map(x => input.clone())
      feedbackCalculateRecursive(phaseSetting, inputs)
    }.max
  }

  def feedbackCalculateRecursive(phaseSetting: Array[Int], inputs: Array[Array[Int]]): Int = {
    def internalFeedbackCalculateRecursive(signal: Int, indexes: Array[Int], initalizationDone: Boolean): Int = {
      val (acc, continuationIndexes) = (indexes, phaseSetting, inputs).zipped
        .foldLeft(signal, Array.empty[Int]) {
          case ((signalAcc, subIndexes), (index, ampPhaseSetting, input)) =>
            val userInputs =
              if (initalizationDone) Iterator(signalAcc) else Iterator(ampPhaseSetting, signalAcc)
            val (ans, subIndex) =
              Day5.processInstruction(input, index, userInputs)
            (ans, subIndexes :+ subIndex)
        }
      if (continuationIndexes.exists(x => x == Int.MaxValue)) {
        return acc
      }
      return internalFeedbackCalculateRecursive(acc, continuationIndexes, true)
    }
    internalFeedbackCalculateRecursive(0, Array.fill(phaseSetting.length)(0), false)
  }
}
