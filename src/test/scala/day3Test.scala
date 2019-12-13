import org.scalatest.FunSuite

class Day3Test extends FunSuite {
  test("Day3Test test getEdge") {
    assert(
      DayImpl.getEdge(Point(0, 0), "R34", 0) === Edge(Point(0, 0), Point(34, 0), 0)
    )
    assert(
      DayImpl.getEdge(Point(0, 0), "L34", 0) === Edge(Point(0, 0), Point(-34, 0), 0)
    )
    assert(
      DayImpl.getEdge(Point(10, 0), "R34", 0) === Edge(Point(10, 0), Point(44, 0), 0)
    )
    assert(
      DayImpl.getEdge(Point(10, 0), "U34", 10) === Edge(Point(10, 0), Point(10, 34), 10)
    )
    assert(
      DayImpl
        .getEdge(Point(10, 0), "D34", 30) === Edge(Point(10, 0), Point(10, -34), 30)
    )
  }

  test("getEdges should return all edges") {
    assert(
      DayImpl.getEdges(Array("R24")) === Seq(Edge(Point(0, 0), Point(24, 0), 0))
    )
    assert(
      DayImpl.getEdges(Array("R24", "U34", "D10", "L14")) === Seq(
        Edge(Point(0, 0), Point(24, 0), 0),
        Edge(Point(24, 0), Point(24, 34), 24),
        Edge(Point(24, 34), Point(24, 24), 58),
        Edge(Point(24, 24), Point(10, 24), 68)
      )
    )
  }

  test("closest Distance1") {
    val wire1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
    val wire2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    val edges1 = DayImpl.getEdges(wire1)
    val edges2 = DayImpl.getEdges(wire2)
    assert(DayImpl.closestDistance(edges1, edges2) === 159)
  }

  test("closest Distance2") {
    val wire1 = "R8,U5,L5,D3".split(",")
    val wire2 = "U7,R6,D4,L4".split(",")
    val edges1 = DayImpl.getEdges(wire1)
    val edges2 = DayImpl.getEdges(wire2)
    assert(DayImpl.closestDistance(edges1, edges2) === 6)
  }

  test("closest Distance3") {
    val wire1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(",")
    val wire2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",")
    val edges1 = DayImpl.getEdges(wire1)
    val edges2 = DayImpl.getEdges(wire2)
    assert(DayImpl.closestDistance(edges1, edges2) === 135)
  }

  test("closest Steps1") {
    val wire1 = "R8,U5,L5,D3".split(",")
    val wire2 = "U7,R6,D4,L4".split(",")
    val edges1 = DayImpl.getEdges(wire1)
    val edges2 = DayImpl.getEdges(wire2)
    assert(DayImpl.closestSteps(edges1, edges2) === (Some(Point(6, 5)), 30))
    DayImpl.getEdges(wire2).foreach(println)
  }
  
  test("closest Steps2") {
    val wire1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
    val wire2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    val edges1 = DayImpl.getEdges(wire1)
    val edges2 = DayImpl.getEdges(wire2)
    assert(DayImpl.closestSteps(edges1, edges2) === (Some(Point(158,-12)), 610))
    DayImpl.getEdges(wire2).foreach(println)
  }
  test("closest Steps3") {
    val wire1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(",")
    val wire2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",")
    val edges1 = DayImpl.getEdges(wire1)
    val edges2 = DayImpl.getEdges(wire2)
    assert(DayImpl.closestSteps(edges1, edges2) === (Some(Point(107,47)), 410))
    DayImpl.getEdges(wire2).foreach(println)
  }
}
