import org.scalatest.FunSuite

class Day6Test extends FunSuite {
  test("Find Direct and Indirect Orbits") {
    val input = """
        COM)B
        B)C
        C)D
        D)E
        E)F
        B)G
        G)H
        D)I
        E)J
        J)K
        K)L""".split(" +").map(_.trim()).filterNot(_ == "")
    assert(Day6.orbitCount(input) == 42)
  }

  test("Minimum Number of Orbits Transfers required") {
    val input = """
        COM)B
        B)C
        C)D
        D)E
        E)F
        B)G
        G)H
        D)I
        E)J
        J)K
        K)L
        K)YOU
        I)SAN""".split(" +").map(_.trim()).filterNot(_ == "")
    assert(Day6.orbitsSwapCount(input) == 4)
  }
}
