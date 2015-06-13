package shackle

import org.scalatest.FunSuite
import scala.collection.immutable.ListMap
import shackle.Constraints._

class ConstraintSolverSuite extends FunSuite {
  test("basic allDiff solution") {
    val domains = ListMap(
      "a" -> List(3),
      "b" -> List(1, 2, 3),
      "c" -> List(2),
      "d" -> List(1, 2, 3, 4))
    val constraints = Seq(allDifferent(vars = List("a", "b", "c", "d")))
    val solver = new ConstraintSolver(domains, constraints)
    val solution = solver.solve()
    assert(solution === Some(ListMap("a" -> 3, "b" -> 1, "c" -> 2, "d" -> 4)))
  }

  test("basic sum solution") {
    val domains = ListMap(
      "a" -> List(3),
      "b" -> List(1, 2, 3),
      "c" -> List(2),
      "d" -> List(1, 2, 3, 4))
    val constraints = Seq(sum(targetValue = 10, vars = List("a", "b", "c", "d")))
    val solver = new ConstraintSolver(domains, constraints)
    val solution = solver.solve()
    assert(solution === Some(ListMap("a" -> 3, "b" -> 1, "c" -> 2, "d" -> 4)))
  }
}
