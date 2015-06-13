package shackle

import scala.collection.immutable.ListMap
import scala.collection.mutable

class ConstraintSolver(
    val domains: ListMap[String, Seq[Any]],
    val constraints: Seq[Constraint],
    val verbose: Boolean = false) {

  type Assignment = ListMap[String, Any]

  def solve(): Option[Assignment] = backtrack(ListMap())

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

trait Constraint {
  def isConsistent(
     assignment: ListMap[String, Any],
     varDomains: ListMap[String, Seq[Any]]): Boolean
}

case class SumConstraint(
    targetValue: Double,
    vars: Seq[String],
    coeffs: Map[String, Double])
    extends Constraint {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    require(vars.forall(v => coeffs.contains(v)),
        "found one or more undefined coefficients")
    var minSum = 0.0
    var maxSum = 0.0
    for (variable <- vars) {
      if (assignment.contains(variable)) {
        val value = coeffs(variable) *
            Constraints.asDouble(assignment(variable))
        minSum += value
        maxSum += value
      } else {
        val domain = varDomains(variable)
        minSum += domain.map(Constraints.asDouble _).min
        maxSum += domain.map(Constraints.asDouble _).max
      }
    }
    targetValue >= minSum && targetValue <= maxSum
  }
}

case class AllDifferentConstraint(
    vars: Seq[String],
    key: (Any => Any))
    extends Constraint {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {

    val usedValues = mutable.Set[Any]()

    // Check for duplicates among assigned values
    for (variable <- vars) {
      if (assignment.contains(variable)) {
        val currentValue = key(assignment(variable))
        if (usedValues.contains(currentValue)) {
          return false
        }
        usedValues.add(currentValue)
      }
    }

    // Check for unassigned values with no remaining options
    for (variable <- vars) {
      if (!assignment.contains(variable)) {
        val domain = varDomains(variable)
        val mappedDomain = domain.map(key).toSet
        val availableValues = mappedDomain - usedValues
        if (availableValues.size == 0) {
          return false
        }
      }
    }

    return true
  }
}

object Constraints {
  def sum(targetValue: Double, vars: Seq[String]): Constraint = {
    val coeffs = vars.map { v => (v, 1.0) }.toMap
    sum(targetValue, vars, coeffs)
  }

  def sum(
      targetValue: Double,
      vars: Seq[String],
      coeffs: Map[String, Double]): Constraint = {
    SumConstraint(targetValue, vars, coeffs)
  }

  def allDifferent(
      vars: Seq[String],
      key: (Any => Any) = identity[Any]): Constraint = {
    AllDifferentConstraint(vars, key)
  }

  private[shackle] def asDouble(x: Any) = x match {
    case y : Double => y
    case y : Int => y.toDouble
    case y : Integer => y.toDouble
    case y : String => y.toDouble
    case y => throw new IllegalArgumentException("Cannot convert to double")
  }
}
