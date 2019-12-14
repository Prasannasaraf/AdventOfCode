object Day4 {
  def hasAdjacentDigits(input: Int): Boolean = {
    val acc = input
      .toString()
      .foldLeft('D', 0)(
        (previousDigit, newDigit) => {
          if (newDigit != previousDigit._1)
            if (previousDigit._2 == 2)
              return true
            else (newDigit, 1)
          else
            (previousDigit._1, previousDigit._2 + 1)
        }
      )
    if (acc._2 == 2) {
      true
    } else false
  }

  def neverDecrease(input: Int): Boolean = {
    input
      .toString()
      .foldLeft(0)(
        (prevDigit, newDigit) => if (prevDigit > newDigit.toInt) return false else newDigit
      )
    true
  }

  def main(args: Array[String]) = {
    val numbers = 123257 to 647015
    val numberOfAcceptNumbers = numbers.filter(x => hasAdjacentDigits(x) && neverDecrease(x)).length
    println(numberOfAcceptNumbers)
  }
}
