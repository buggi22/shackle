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

private[shackle] case class SumConstraint(
    targetValue: Double,
    vars: Seq[String],
    coeffs: Map[String, Double])
    extends Constraint with NumericUtil {
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

private[shackle] case class SameValueConstraint(
    var1: String,
    var2: String)
    extends Constraint {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    (assignment.get(var1), assignment.get(var2)) match {
      case (Some(val1), Some(val2)) => val1 == val2
      case _ => true
    }
  }
}

private[shackle] case class ValueIsConstraint(
    variable: String,
    targetValue: Any)
    extends Constraint {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    assignment.get(variable) match {
      case Some(value) => value == targetValue
      case _ => true
    }
  }
}

private[shackle] case class DifferenceConstraint(
    var1: String,
    var2: String,
    targetDifference: Double)
    extends Constraint with NumericUtil {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    (assignment.get(var1), assignment.get(var2)) match {
      case (Some(val1), Some(val2)) =>
        asDouble(val1) - asDouble(val2) == targetDifference
      case _ => true
    }
  }
}

private[shackle] case class AbsoluteDifferenceConstraint(
    var1: String,
    var2: String,
    targetDifference: Double)
    extends Constraint with NumericUtil {
  override def isConsistent(
      assignment: ListMap[String, Any],
      varDomains: ListMap[String, Seq[Any]]): Boolean = {
    (assignment.get(var1), assignment.get(var2)) match {
      case (Some(val1), Some(val2)) =>
        math.abs(asDouble(val1) - asDouble(val2)) == math.abs(targetDifference)
      case _ => true
    }
  }
}

