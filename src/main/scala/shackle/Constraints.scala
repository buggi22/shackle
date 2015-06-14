package shackle

import scala.collection.immutable.ListMap

trait Constraint {
  def isConsistent(
     assignment: ListMap[String, Any],
     varDomains: ListMap[String, Seq[Any]]): Boolean
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

  def sameValue(var1: String, var2: String): Constraint = {
    SameValueConstraint(var1, var2)
  }

  def valueIs(variable: String, value: Any): Constraint = {
    ValueIsConstraint(variable, value)
  }

  def difference(var1: String, var2: String, targetDifference: Double): Constraint = {
    DifferenceConstraint(var1, var2, targetDifference)
  }

  def absoluteDifference(var1: String, var2: String, targetDifference: Double): Constraint = {
    AbsoluteDifferenceConstraint(var1, var2, targetDifference)
  }
}
