package shackle.sat

import scala.collection.immutable.ListMap

import shackle.{ConstraintSolver, Constraints}

object SatSolver {
  def apply(vars: Seq[String], expr: Expression): SatSolver = {
    new SatSolver(vars, expr)
  }

  type Assignment = Map[String, Boolean]
}

class SatSolver(val vars: Seq[String], val expr: Expression) {

  type Assignment = SatSolver.Assignment

  // NOTE: this implementation uses a backtracking constraint solver
  // TODO: use a more efficient SAT solver
  val constraintSolver: ConstraintSolver = {
    val domains = ListMap(vars.map { v => (v, List(true, false)) } : _*)
    val constraints = List(Constraints.partialMapPredicate(vars, { assignment =>
      val boolAssignment = assignment.mapValues(_.asInstanceOf[Boolean])
      val result = Expressions.partialEval(expr, boolAssignment)
      result match {
        case Some(true) => true
        case Some(false) => false
        case None => true
        // Note: Assignment remains consistent until it is provably false
      }
    }))
    ConstraintSolver(domains, constraints)
  }

  private[sat] def convertFromConstraintSolution(
      solution: ConstraintSolver.Assignment): Assignment = {
    solution.map { case (k, v) => (k, v.asInstanceOf[Boolean]) }
  }

  def solve: Option[Assignment] =
      constraintSolver.solve.map(convertFromConstraintSolution _)

  def allSolutions: Iterator[Assignment] =
      constraintSolver.allSolutions.map(convertFromConstraintSolution _)
}
