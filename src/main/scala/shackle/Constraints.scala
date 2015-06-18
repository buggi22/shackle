package shackle

import scala.collection.immutable.ListMap

trait Constraint {
  def isConsistent(
     assignment: ListMap[String, Any],
     varDomains: ListMap[String, Seq[Any]]): Boolean
}

object Constraints extends NumericUtil {
  def sum(targetValue: Double, vars: Seq[String]): Constraint = {
    val coeffs = vars.map { v => (v, 1.0) }.toMap
    sum(targetValue, vars, coeffs)
  }

  def allDifferent(
      vars: Seq[String],
      key: (Any => Any) = identity[Any]): Constraint = {
    AllDifferentConstraint(vars, key)
  }

  def sameValue(var1: String, var2: String): Constraint = {
    binaryPredicate(var1, var2,
        (x1: Any, x2: Any) => x1 == x2)
  }

  def valueIs(variable: String, value: Any): Constraint = {
    unaryPredicate(variable,
        (x: Any) => x == value)
  }

  def valueIsNot(variable: String, value: Any): Constraint = {
    unaryPredicate(variable,
        (x: Any) => x != value)
  }

  def sum(
      targetValue: Double,
      vars: Seq[String],
      coeffs: Map[String, Double]): Constraint = {
    SumConstraint(targetValue, vars, coeffs)
  }

  def difference(var1: String, var2: String, targetValue: Double): Constraint = {
    binaryNumericPredicate(var1, var2,
        (x1: Double, x2: Double) => x1 - x2 == targetValue)
  }

  def absoluteDifference(
      var1: String,
      var2: String,
      targetValue: Double): Constraint = {
    binaryNumericPredicate(var1, var2,
        (x1: Double, x2: Double) => math.abs(x1 - x2) == targetValue)
  }

  def product(vars: Seq[String], targetValue: Double): Constraint = {
    seqNumericPredicate(vars, xs => xs.product == targetValue)
  }

  def seqPredicate(
      vars: Seq[String],
      predicate: Seq[Any] => Boolean): Constraint = {
    GenericPredicateConstraint(vars, predicate)
  }

  def mapPredicate(
      vars: Seq[String],
      predicate: Map[String, Any] => Boolean): Constraint = {
    seqPredicate(vars, xs => predicate(vars.zip(xs).toMap))
  }

  def partialMapPredicate(
      vars: Seq[String],
      predicate: Map[String, Any] => Boolean): Constraint = {
    PartialMapPredicateConstraint(vars, predicate)
  }

  def unaryPredicate(
      variable: String,
      predicate: Any => Boolean): Constraint = {
    seqPredicate(Seq(variable), xs => predicate(xs(0)))
  }

  def binaryPredicate(
      var1: String,
      var2: String,
      predicate: (Any, Any) => Boolean): Constraint = {
    seqPredicate(Seq(var1, var2), xs => predicate(xs(0), xs(1)))
  }

  def seqNumericPredicate(
      vars: Seq[String],
      predicate: Seq[Double] => Boolean): Constraint = {
    seqPredicate(vars, xs => predicate(xs.map(asDouble _)))
  }

  def mapNumericPredicate(
      vars: Seq[String],
      predicate: Map[String, Double] => Boolean): Constraint = {
    mapPredicate(vars, { assignment =>
      predicate(assignment.mapValues(asDouble _))
    })
  }

  def partialMapNumericPredicate(
      vars: Seq[String],
      predicate: Map[String, Double] => Boolean): Constraint = {
    partialMapPredicate(vars, { assignment =>
      predicate(assignment.mapValues(asDouble _))
    })
  }

  def unaryNumericPredicate(
      variable: String,
      predicate: Double => Boolean): Constraint = {
    unaryPredicate(variable, x => predicate(asDouble(x)))
  }

  def binaryNumericPredicate(
      var1: String,
      var2: String,
      predicate: (Double, Double) => Boolean): Constraint = {
    binaryPredicate(var1, var2,
        (x1, x2) => predicate(asDouble(x1), asDouble(x2)))
  }
}
