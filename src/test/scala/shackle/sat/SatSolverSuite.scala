package shackle.sat

import org.scalatest.FunSuite
import scala.collection.immutable.ListMap
import shackle.sat.Expressions._

class SatSolverSuite extends FunSuite {

  def checkPartialTruthTable[T](
      expr: T,
      vars: Seq[String],
      table: ListMap[Seq[Boolean], Boolean],
      evalFunc: (T, SatSolver.Assignment) => Boolean): Unit = {
    var foundMismatch = false
    val results = for ((inputs, expectedOutput) <- table) yield {
      require(inputs.size == vars.size,
          s"test setup error: sizes do not match: ${vars} vs ${inputs}")
      val assignment = vars.zip(inputs).toMap
      val actualOutput = evalFunc(expr, assignment)
      if (expectedOutput != actualOutput) {
        foundMismatch = true
      }
      (assignment, expectedOutput, actualOutput)
    }
    if (foundMismatch) {
      var message = results.map { case (assignment, expected, actual) =>
        s"$assignment: expected $expected; actual $actual"
      }.mkString("\n")
      fail(s"Found mismatch in partial truth table for ${expr}:\n${message}")
    }
  }

  val cnfEval = { (cnf: ConjunctiveNormal, assignment: SatSolver.Assignment) =>
    eval(cnf, assignment)
  }

  val exprEval = { (expr: Expression, assignment: SatSolver.Assignment) =>
    eval(expr, assignment)
  }

  val emptyAssignment = ListMap[String, Boolean]()

  test("conjunctive normal form - one positive literal") {
    val expr = lit("x")
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(clause(term("x", true))))

    val truthTable = ListMap(
      Seq(true) -> true,
      Seq(false) -> false)
    checkPartialTruthTable(expr, List("x"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x"), truthTable, cnfEval)
  }

  test("conjunctive normal form - one negative literal") {
    val expr = not(lit("x"))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(clause(term("x", false))))

    val truthTable = ListMap(
      Seq(true) -> false,
      Seq(false) -> true)
    checkPartialTruthTable(expr, List("x"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x"), truthTable, cnfEval)
  }

  test("conjunctive normal form - simple or") {
    val expr = or(lit("x"), not(lit("y")), lit("z"))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(clause(
        term("x", true), term("y", false), term("z", true))))

    // Check truth table - note that "y" appears as a negated term
    val truthTable = ListMap(
      Seq(true, false, true) -> true,
      Seq(true, true, false) -> true,
      Seq(false, false, false) -> true,
      Seq(false, true, true) -> true,
      Seq(true, false, false) -> true,
      Seq(true, true, true) -> true,
      Seq(false, false, true) -> true,
      Seq(false, true, false) -> false)
    checkPartialTruthTable(expr, List("x", "y", "z"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y", "z"), truthTable, cnfEval)
  }

  test("conjunctive normal form - always false") {
    val expr = alwaysFalse
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(clause()))

    val truthTable = ListMap(
      Seq(true) -> false,
      Seq(false) -> false)
    checkPartialTruthTable(expr, List("x"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x"), truthTable, cnfEval)
  }

  test("conjunctive normal form - always true") {
    val expr = alwaysTrue
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal())

    val truthTable = ListMap(
      Seq(true) -> true,
      Seq(false) -> true)
    checkPartialTruthTable(expr, List("x"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x"), truthTable, cnfEval)
  }

  test("conjunctive normal form - double negation") {
    val expr = not(not(lit("x")))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(clause(term("x", true))))

    val truthTable = ListMap(
      Seq(true) -> true,
      Seq(false) -> false)
    checkPartialTruthTable(expr, List("x"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x"), truthTable, cnfEval)
  }

  test("conjunctive normal form - simple and") {
    val expr = and(lit("x"), not(lit("y")), lit("z"))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(
        clause(term("x", true)),
        clause(term("y", false)),
        clause(term("z", true))))

    // Check truth table - note that "y" appears as a negated term
    val truthTable = ListMap(
      Seq(true, false, true) -> true,
      Seq(true, true, false) -> false,
      Seq(false, false, false) -> false,
      Seq(false, true, true) -> false,
      Seq(true, false, false) -> false,
      Seq(true, true, true) -> false,
      Seq(false, false, true) -> false,
      Seq(false, true, false) -> false)
    checkPartialTruthTable(expr, List("x", "y", "z"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y", "z"), truthTable, cnfEval)
  }

  test("conjunctive normal form - and(or)") {
    val expr = and(
        or(lit("a"), lit("b"), not(lit("c"))),
        or(lit("d"), not(lit("e")), lit("f")),
        or(not(lit("g")), lit("h"), lit("i")))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(
        clause(term("a", true), term("b", true), term("c", false)),
        clause(term("d", true), term("e", false), term("f", true)),
        clause(term("g", false), term("h", true), term("i", true))))
  }

  test("conjunctive normal form - or(and) - distributive law") {
    val expr = or(
        and(lit("a"), lit("b"), not(lit("c"))),
        and(lit("d"), not(lit("e")), lit("f")),
        and(not(lit("g")), lit("h"), lit("i")))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(
        clause(term("a", true), term("d", true), term("g", false)),
        clause(term("a", true), term("d", true), term("h", true)),
        clause(term("a", true), term("d", true), term("i", true)),
        clause(term("a", true), term("e", false), term("g", false)),
        clause(term("a", true), term("e", false), term("h", true)),
        clause(term("a", true), term("e", false), term("i", true)),
        clause(term("a", true), term("f", true), term("g", false)),
        clause(term("a", true), term("f", true), term("h", true)),
        clause(term("a", true), term("f", true), term("i", true)),

        clause(term("b", true), term("d", true), term("g", false)),
        clause(term("b", true), term("d", true), term("h", true)),
        clause(term("b", true), term("d", true), term("i", true)),
        clause(term("b", true), term("e", false), term("g", false)),
        clause(term("b", true), term("e", false), term("h", true)),
        clause(term("b", true), term("e", false), term("i", true)),
        clause(term("b", true), term("f", true), term("g", false)),
        clause(term("b", true), term("f", true), term("h", true)),
        clause(term("b", true), term("f", true), term("i", true)),

        clause(term("c", false), term("d", true), term("g", false)),
        clause(term("c", false), term("d", true), term("h", true)),
        clause(term("c", false), term("d", true), term("i", true)),
        clause(term("c", false), term("e", false), term("g", false)),
        clause(term("c", false), term("e", false), term("h", true)),
        clause(term("c", false), term("e", false), term("i", true)),
        clause(term("c", false), term("f", true), term("g", false)),
        clause(term("c", false), term("f", true), term("h", true)),
        clause(term("c", false), term("f", true), term("i", true))))
  }

  test("conjunctive normal form - not(or)") {
    val expr = not(or(lit("a"), lit("b"), not(lit("c"))))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(
        clause(term("a", false)),
        clause(term("b", false)),
        clause(term("c", true))))
  }

  test("conjunctive normal form - not(and)") {
    val expr = not(and(lit("a"), lit("b"), not(lit("c"))))
    val cnf = asConjunctiveNormal(expr)
    assert(cnf === conjunctiveNormal(
        clause(term("a", false), term("b", false), term("c", true))))
  }

  test("conjunctive normal form - xor") {
    val expr = xor(lit("x"), lit("y"))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true) -> false,
        Seq(true, false) -> true,
        Seq(false, true) -> true,
        Seq(false, false) -> false)
    checkPartialTruthTable(expr, List("x", "y"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y"), truthTable, cnfEval)
  }

  test("conjunctive normal form - equiv") {
    val expr = equiv(lit("x"), lit("y"))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true) -> true,
        Seq(true, false) -> false,
        Seq(false, true) -> false,
        Seq(false, false) -> true)
    checkPartialTruthTable(expr, List("x", "y"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y"), truthTable, cnfEval)
  }

  test("conjunctive normal form - implies") {
    val expr = implies(lit("x"), lit("y"))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true) -> true,
        Seq(true, false) -> false,
        Seq(false, true) -> true,
        Seq(false, false) -> true)
    checkPartialTruthTable(expr, List("x", "y"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y"), truthTable, cnfEval)
  }

  test("conjunctive normal form - numberTrue(0, [x, y, z])") {
    val expr = numberTrue(0, Seq(lit("x"), lit("y"), lit("z")))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true, true) -> false,
        Seq(true, true, false) -> false,
        Seq(true, false, true) -> false,
        Seq(true, false, false) -> false,
        Seq(false, true, true) -> false,
        Seq(false, true, false) -> false,
        Seq(false, false, true) -> false,
        Seq(false, false, false) -> true)
    checkPartialTruthTable(expr, List("x", "y", "z"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y", "z"), truthTable, cnfEval)
  }

  test("conjunctive normal form - numberTrue(1, [x, y, z])") {
    val expr = numberTrue(1, Seq(lit("x"), lit("y"), lit("z")))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true, true) -> false,
        Seq(true, true, false) -> false,
        Seq(true, false, true) -> false,
        Seq(true, false, false) -> true,
        Seq(false, true, true) -> false,
        Seq(false, true, false) -> true,
        Seq(false, false, true) -> true,
        Seq(false, false, false) -> false)
    checkPartialTruthTable(expr, List("x", "y", "z"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y", "z"), truthTable, cnfEval)
  }

  test("conjunctive normal form - numberTrue(2, [x, y, z])") {
    val expr = numberTrue(2, Seq(lit("x"), lit("y"), lit("z")))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true, true) -> false,
        Seq(true, true, false) -> true,
        Seq(true, false, true) -> true,
        Seq(true, false, false) -> false,
        Seq(false, true, true) -> true,
        Seq(false, true, false) -> false,
        Seq(false, false, true) -> false,
        Seq(false, false, false) -> false)
    checkPartialTruthTable(expr, List("x", "y", "z"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y", "z"), truthTable, cnfEval)
  }

  test("conjunctive normal form - numberTrue(3, [x, y, z])") {
    val expr = numberTrue(3, Seq(lit("x"), lit("y"), lit("z")))
    val cnf = asConjunctiveNormal(expr)

    val truthTable = ListMap(
        Seq(true, true, true) -> true,
        Seq(true, true, false) -> false,
        Seq(true, false, true) -> false,
        Seq(true, false, false) -> false,
        Seq(false, true, true) -> false,
        Seq(false, true, false) -> false,
        Seq(false, false, true) -> false,
        Seq(false, false, false) -> false)
    checkPartialTruthTable(expr, List("x", "y", "z"), truthTable, exprEval)
    checkPartialTruthTable(cnf, List("x", "y", "z"), truthTable, cnfEval)
  }

  test("partialEval - alwaysTrue / alwaysFalse") {
    assert(partialEval(alwaysTrue, emptyAssignment) === Some(true))
    assert(partialEval(alwaysTrue, ListMap("x" -> true)) === Some(true))
    assert(partialEval(alwaysTrue, ListMap("x" -> false)) === Some(true))
    assert(partialEval(alwaysFalse, emptyAssignment) === Some(false))
    assert(partialEval(alwaysFalse, ListMap("x" -> true)) === Some(false))
    assert(partialEval(alwaysFalse, ListMap("x" -> false)) === Some(false))
  }

  test("partialEval - literal") {
    assert(partialEval(lit("x"), ListMap("x" -> true)) === Some(true))
    assert(partialEval(lit("x"), ListMap("x" -> false)) === Some(false))
    assert(partialEval(lit("x"), ListMap("y" -> true)) === None)
    assert(partialEval(lit("x"), emptyAssignment) === None)
  }

  test("partialEval - and") {
    val e = and(lit("x"), lit("y"))

    assert(partialEval(e, emptyAssignment) === None)
    assert(partialEval(e, ListMap("z" -> true)) === None)

    assert(partialEval(e, ListMap("x" -> true)) === None)
    assert(partialEval(e, ListMap("x" -> false)) === Some(false))
    assert(partialEval(e, ListMap("y" -> true)) === None)
    assert(partialEval(e, ListMap("y" -> false)) === Some(false))

    assert(partialEval(e, ListMap("x" -> true, "y" -> true)) === Some(true))
    assert(partialEval(e, ListMap("x" -> false, "y" -> true)) === Some(false))
    assert(partialEval(e, ListMap("x" -> true, "y" -> false)) === Some(false))
    assert(partialEval(e, ListMap("x" -> false, "y" -> false)) === Some(false))
  }

  test("partialEval - or") {
    val e = or(lit("x"), lit("y"))

    assert(partialEval(e, emptyAssignment) === None)
    assert(partialEval(e, ListMap("z" -> true)) === None)

    assert(partialEval(e, ListMap("x" -> true)) === Some(true))
    assert(partialEval(e, ListMap("x" -> false)) === None)
    assert(partialEval(e, ListMap("y" -> true)) === Some(true))
    assert(partialEval(e, ListMap("y" -> false)) === None)

    assert(partialEval(e, ListMap("x" -> true, "y" -> true)) === Some(true))
    assert(partialEval(e, ListMap("x" -> false, "y" -> true)) === Some(true))
    assert(partialEval(e, ListMap("x" -> true, "y" -> false)) === Some(true))
    assert(partialEval(e, ListMap("x" -> false, "y" -> false)) === Some(false))
  }

  test("partialEval - not") {
    assert(partialEval(not(lit("x")), ListMap("x" -> true)) === Some(false))
    assert(partialEval(not(lit("x")), ListMap("x" -> false)) === Some(true))
    assert(partialEval(not(lit("x")), ListMap("y" -> true)) === None)
    assert(partialEval(not(lit("x")), emptyAssignment) === None)
  }

  test("partialEval - xor") {
    val e = xor(lit("x"), lit("y"))

    assert(partialEval(e, emptyAssignment) === None)
    assert(partialEval(e, ListMap("z" -> true)) === None)

    assert(partialEval(e, ListMap("x" -> true)) === None)
    assert(partialEval(e, ListMap("x" -> false)) === None)
    assert(partialEval(e, ListMap("y" -> true)) === None)
    assert(partialEval(e, ListMap("y" -> false)) === None)

    assert(partialEval(e, ListMap("x" -> true, "y" -> true)) === Some(false))
    assert(partialEval(e, ListMap("x" -> false, "y" -> true)) === Some(true))
    assert(partialEval(e, ListMap("x" -> true, "y" -> false)) === Some(true))
    assert(partialEval(e, ListMap("x" -> false, "y" -> false)) === Some(false))
  }

  test("SatSolver - simple and(or)") {
    val expr = and(
        or(lit("a"), lit("b")),
        or(not(lit("a")), not(lit("b"))))
    val vars = List("a", "b")
    val solver = SatSolver(vars, expr)
    val allSolutions = solver.allSolutions.toList
    assert(allSolutions === List(
        Map("a" -> true, "b" -> false),
        Map("a" -> false, "b" -> true)))
  }

}
