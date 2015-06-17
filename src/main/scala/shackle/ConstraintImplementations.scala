package shackle

import scala.collection.immutable.ListMap
import scala.collection.mutable

private[shackle] trait NumericUtil {
  private[shackle] def asDouble(x: Any) = x match {
    case y : Double => y
    case y : Int => y.toDouble
    case y : Integer => y.toDouble
    case y : String => y.toDouble
    case y => throw new IllegalArgumentException("Cannot convert to double")
  }
}

private[shackle] case class AllDifferentConstraint(
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

private[shackle] case class GenericPredicateConstraint(
    vars: Seq[String],
    predicate: Seq[Any] => Boolean)
    extends Constraint {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    if (vars.exists { v => !assignment.get(v).isDefined }) {
      // We can only conclude that the constraint is violated if all
      // involved variables have been assigned.
      true
    } else {
      val values = vars.map { v => assignment.get(v).get }
      predicate(values)
    }
  }
}

private[shackle] case class SumConstraint(
    targetValue: Double,
    vars: Seq[String],
    coeffs: Map[String, Double])
    extends Constraint with NumericUtil {

  require(vars.forall(v => coeffs.contains(v)),
      "found one or more undefined coefficients")

  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    var minSum = 0.0
    var maxSum = 0.0
    for (variable <- vars) {
      if (assignment.contains(variable)) {
        val value = coeffs(variable) *
            asDouble(assignment(variable))
        minSum += value
        maxSum += value
      } else {
        val domain = varDomains(variable)
        minSum += domain.map(asDouble _).min
        maxSum += domain.map(asDouble _).max
      }
    }
    targetValue >= minSum && targetValue <= maxSum
  }
}

private[shackle] case class DifferentSolutionConstraint(
    solution: ConstraintSolver.Assignment)
    extends Constraint {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    if (assignment.size < solution.size) {
      // There are more assignments available, so we can't eliminate this
      // solution yet
      true
    } else {
      // This constraint is satisfied only if the solution differs
      assignment != solution
    }
  }
}
