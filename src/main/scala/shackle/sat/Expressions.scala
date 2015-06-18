package shackle.sat

sealed trait Expression {}
case class Literal(name: String) extends Expression
case class And(exprs: Seq[Expression]) extends Expression
case class Or(exprs: Seq[Expression]) extends Expression
case class Not(expr: Expression) extends Expression

object Expressions {

  def lit(name: String): Expression = Literal(name)

  def literal(name: String): Expression = Literal(name)

  def alwaysFalse: Expression = Or(List())

  def alwaysTrue: Expression = And(List())

  def and(exprs: Expression*): Expression = And(exprs.toList)

  def or(exprs: Expression*): Expression = Or(exprs.toList)

  def not(expr: Expression): Expression = Not(expr)

  def xor(expr1: Expression, expr2: Expression): Expression = {
    and(or(expr1, expr2), not(and(expr1, expr2)))
  }

  def equiv(expr1: Expression, expr2: Expression): Expression = {
    and(implies(expr1, expr2), implies(expr2, expr1))
  }

  def implies(expr1: Expression, expr2: Expression): Expression = {
    or(not(expr1), expr2)
  }

  /**
   * An expression that checks whether exactly n of exprs are true.
   */
  def numberTrue(n: Int, exprs: Seq[Expression]): Expression = {
    require(n >= 0, "argument to numberTrue cannot be negative")
    (n, exprs) match {
      case (0, Nil) => alwaysTrue
      case (0, _) => And(exprs.map(not _))
      case _ => {
        if (exprs.size < n) {
          alwaysFalse
        } else if (exprs.size == n) {
          And(exprs)
        } else {
          val indices = 0 until exprs.size
          val clauses = indices.toSet.subsets.withFilter { subset =>
            subset.size != exprs.size - n
          }.map { subset =>
            Or(indices.map { i =>
              if (subset contains i) {
                exprs(i)
              } else {
                not(exprs(i))
              }
            })
          }.toList
          And(clauses)
        }
      }
    }
  }

  def conjunctiveNormal(clauses: Clause*) = ConjunctiveNormal(clauses.toSeq)

  def clause(terms: Term*) = Clause(terms.toSeq)

  def term(name: String, truthValue: Boolean) = Term(name, truthValue)

  private[sat] def disjunctionOfConjunctiveNormals(
      conjunctives: Seq[ConjunctiveNormal]): ConjunctiveNormal = {
    if (conjunctives.isEmpty) {
      // Applying "or" to an empty list of expressions yields
      // a single 0-term clause (which is always false).
      conjunctiveNormal(clause())
    } else if (conjunctives.size == 1) {
      // Applying "or" to a single expression yields the original expression.
      conjunctives.head
    } else if (conjunctives(0).clauses.isEmpty ||
        conjunctives(1).clauses.isEmpty) {
      // An empty "and" statement is always true, so the "or" statement
      // will always be true (represented as an empty set of clauses).
      conjunctiveNormal()
    } else {
      // Use the distributive laws to combine the first two expressions,
      // and continue the evaluation recursively.
      val expr1 = conjunctives(0)
      val expr2 = conjunctives(1)
      val newHeadClauses: Seq[Clause] = (for (
          clause1 <- expr1.clauses;
          clause2 <- expr2.clauses) yield {
        clause1.terms.toSet ++ clause2.terms.toSet
      }).flatMap { terms =>
        val names: Set[String] = terms.map(_.name)
        // If a term appears both as positive and negative in a clause,
        // the entire clause is always true and can be omitted.
        val containsTrueAndFalse = names.size < terms.size
        if (containsTrueAndFalse) {
          None
        } else {
          Some(Clause(terms.toList))
        }
      }
      val newHeadExpr = ConjunctiveNormal(newHeadClauses)
      val remaining = conjunctives.drop(2)
      disjunctionOfConjunctiveNormals(newHeadExpr +: remaining)
    }
  }

  def asConjunctiveNormal(expr: Expression): ConjunctiveNormal = {
    expr match {
      case Literal(name) => {
        val term = Term(name, true)
        ConjunctiveNormal(Seq(Clause(Seq(term))))
      }
      case And(exprs) => {
        val clauses = exprs.flatMap { e => asConjunctiveNormal(e).clauses }
        ConjunctiveNormal(clauses)
      }
      case Or(exprs) => {
        disjunctionOfConjunctiveNormals(exprs.map(asConjunctiveNormal _))
      }
      case Not(exprToNegate) => exprToNegate match {
        case Literal(name) => {
          val term = Term(name, false)
          ConjunctiveNormal(Seq(Clause(Seq(term))))
        }
        case Not(inner) => {
          // Double negation
          asConjunctiveNormal(inner)
        }
        case And(innerExprs) => {
          val innerConjunctiveNormalForms: Seq[ConjunctiveNormal] =
              innerExprs.map(e => asConjunctiveNormal(not(e)))
          disjunctionOfConjunctiveNormals(innerConjunctiveNormalForms)
        }
        case Or(innerExprs) => {
          asConjunctiveNormal(And(innerExprs.map(x => Not(x))))
        }
      }
    }
  }

  def eval(cnf: ConjunctiveNormal,
      assignment: SatSolver.Assignment): Boolean = {
    val result = partialEval(cnf, assignment)
    require(result.isDefined,
        "Unable to evaluate expression (incomplete assignment)")
    result.get
  }

  def partialEval(cnf: ConjunctiveNormal,
     assignment: SatSolver.Assignment): Option[Boolean] = {

    val clauseResults = cnf.clauses.map { clause =>
      lazy val isCertainlyTrue = clause.terms.exists { term =>
        assignment.get(term.name) == Some(term.truthValue)
      }

      lazy val couldBeTrue = clause.terms.exists { term =>
        assignment.get(term.name) == None
      }

      if (isCertainlyTrue) {
        Some(true)
      } else if (couldBeTrue) {
        None
      } else {
        Some(false)
      }
    }

    lazy val isCertainlyTrue = clauseResults.forall { r => r == Some(true) }

    lazy val couldBeTrue = clauseResults.forall { r => r != Some(false) }

    if (isCertainlyTrue) {
      Some(true)
    } else if (couldBeTrue) {
      None
    } else {
      Some(false)
    }
  }

  def eval(expr: Expression,
      assignment: SatSolver.Assignment): Boolean = {
    val result = partialEval(expr, assignment)
    require(result.isDefined,
        "Unable to evaluate expression (incomplete assignment)")
    result.get
  }

  def partialEval(expr: Expression,
      assignment: SatSolver.Assignment): Option[Boolean] = expr match {

    case Literal(name) => assignment.get(name)

    case Not(inner) => partialEval(inner, assignment).map(x => !x)

    case And(inners) => {
      val results = inners.map { subExpr => partialEval(subExpr, assignment) }

      lazy val isCertainlyTrue = results.forall { r => r == Some(true) }
      lazy val isCertainlyFalse = results.exists { r => r == Some(false) }

      if (isCertainlyTrue) {
        Some(true)
      } else if (isCertainlyFalse) {
        Some(false)
      } else {
        None
      }
    }

    case Or(inners) => {
      val results = inners.map { subExpr => partialEval(subExpr, assignment) }

      lazy val isCertainlyTrue = results.exists { r => r == Some(true) }
      lazy val isCertainlyFalse = results.forall { r => r == Some(false) }

      if (isCertainlyTrue) {
        Some(true)
      } else if (isCertainlyFalse) {
        Some(false)
      } else {
        None
      }
    }
  }
}

case class Term(name: String, truthValue: Boolean)

case class Clause(terms: Seq[Term])

case class ConjunctiveNormal(clauses: Seq[Clause])

