import org.scalatest.FunSuite

class Day4Test extends FunSuite {
    test("hasAdjacentDigits") {
        assert(Day4.hasAdjacentDigits(123445))
        assert(Day4.hasAdjacentDigits(123345))
        assert(Day4.hasAdjacentDigits(123455))
        assert(!Day4.hasAdjacentDigits(123454))
    }

    test("neverDecrease") {
        assert(Day4.neverDecrease(123445))
        assert(!Day4.neverDecrease(12343))
    }
}