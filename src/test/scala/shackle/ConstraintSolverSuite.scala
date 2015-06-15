package shackle

import org.scalatest.FunSuite
import scala.collection.immutable.ListMap
import shackle.Constraints._

class ConstraintSolverSuite extends FunSuite {
  val simpleDomains = ListMap(
      "a" -> List(3),
      "b" -> List(1, 2, 3),
      "c" -> List(2),
      "d" -> List(1, 2, 3, 4))

  test("basic allDiff constraint - unique solution") {
    val constraints = Seq(
       allDifferent(vars = List("a", "b", "c", "d")))
    val solver = ConstraintSolver(simpleDomains, constraints)
    val solution = solver.solve
    val allSolutions = solver.allSolutions

    val expectedSolution = ListMap("a" -> 3, "b" -> 1, "c" -> 2, "d" -> 4)

    assert(solution === Some(expectedSolution))
    assert(allSolutions.toList === List(expectedSolution))
  }

  test("basic sum constraint - multiple solutions") {
    val constraints = Seq(
       sum(targetValue = 10, vars = List("a", "b", "c", "d")))
    val solver = ConstraintSolver(simpleDomains, constraints)
    val solution = solver.solve
    val allSolutions = solver.allSolutions

    val expectedSolutions = List(
        ListMap("a" -> 3, "b" -> 1, "c" -> 2, "d" -> 4),
        ListMap("a" -> 3, "b" -> 2, "c" -> 2, "d" -> 3),
        ListMap("a" -> 3, "b" -> 3, "c" -> 2, "d" -> 2))

    assert(solution === Some(expectedSolutions(0)))
    assert(allSolutions.toList === expectedSolutions)
  }

  test("no solutions") {
    val constraints = Seq(
        sum(targetValue = 20, vars = List("a", "b", "c", "d")))
    val solver = ConstraintSolver(simpleDomains, constraints)
    val solution = solver.solve
    val allSolutions = solver.allSolutions

    assert(solution === None)
    assert(allSolutions.toList.isEmpty)
  }
}
