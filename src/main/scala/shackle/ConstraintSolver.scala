package shackle

import scala.collection.immutable.ListMap

object ConstraintSolver {
  def apply(
      domains: ListMap[String, Seq[Any]],
      constraints: Seq[Constraint],
      verbose: Boolean = false): ConstraintSolver = {
    new ConstraintSolver(domains, constraints, verbose)
  }

  type Assignment = ListMap[String, Any]
}

class ConstraintSolver(
    val domains: ListMap[String, Seq[Any]],
    val constraints: Seq[Constraint],
    val verbose: Boolean = false) {

  type Assignment = ConstraintSolver.Assignment

  def solve: Option[Assignment] = backtrack(ListMap())

  def allSolutions: Iterator[Assignment] = {
    new Iterator[Assignment] {
      var solver: ConstraintSolver = ConstraintSolver.this
      var nextCached: Option[Assignment] = solver.solve

      override def hasNext: Boolean = nextCached.isDefined

      override def next: Assignment = nextCached match {
        case None => throw new IllegalStateException("No more solutions.")
        case Some(solution) => {
          solver = new ConstraintSolver(
              solver.domains,
              solver.constraints :+ DifferentSolutionConstraint(solution),
              solver.verbose)
          nextCached = solver.solve
          solution
        }
      }
    }
  }

  private def debug(message: => Any): Unit = {
    if (verbose) { println(message) }
  }

  private def backtrack(assignment: Assignment): Option[Assignment] = {
    debug(s"Running backtrack with assignment = ${assignment}")
    if (isComplete(assignment)) {
      return Some(assignment)
    }

    val variable = selectUnassignedVariable(assignment)
    val domain = domains(variable)
    for (value <- domain) {
      val newAssignment = assignment + ((variable, value))
      if (isConsistent(newAssignment)) {
        val result = backtrack(newAssignment)
        if (result.isDefined) {
          return result
        }
      } else {
        debug("Found inconsistency; backtracking")
      }
    }
    return None
  }

  private def isComplete(assignment: Assignment): Boolean = {
    for (variable <- domains.map(_._1)) {
      if (!assignment.contains(variable)) {
        return false
      }
    }
    return true
  }

  private def isConsistent(assignment: Assignment): Boolean = {
    debug(s"Running isConsistent for ${assignment}")
    for (c <- constraints) {
      if (!c.isConsistent(assignment, domains)) {
        debug(s"isConsistent found violation at ${c}")
        return false
      }
    }
    debug(s"isConsistent returning true")
    return true
  }

  private def selectUnassignedVariable(assignment: Assignment): String = {
    for (variable <- domains.map(_._1)) {
      if (!assignment.contains(variable)) {
        return variable
      }
    }
    throw new IllegalStateException(
        "Internal error: no remaining unassigned variables")
  }
}

