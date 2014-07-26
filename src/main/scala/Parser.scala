package net.tokyoenvious.lambdacalc

import util.parsing.combinator._

/**
 * Syntax:
 *   term ::= variable
 *          | term term
 *          | '\' variable '.' term
 */
object Parser extends RegexParsers with PackratParsers {
  lazy val variable: PackratParser[NamedVariable] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ {
    case name => NamedVariable(name)
  }

  lazy val abstraction: PackratParser[NamedAbstraction] = "\\" ~> variable ~ "." ~ term ^^ {
    case (v ~ _ ~ b) => NamedAbstraction(v, b)
  }

  lazy val application: PackratParser[NamedTerm] = rep1(abstraction | variable | parenTerm) ^^ {
    case (t :: ts) => (t /: ts) { NamedApplication }
    case _ => ??? // Could not occur
  }

  lazy val parenTerm: PackratParser[NamedTerm] = "(" ~> term <~ ")"

  lazy val term: PackratParser[NamedTerm] =
    abstraction | application | variable | parenTerm |
      failure("expected '\\', '(' or variable")

  def parse(input: String): ParseResult[NamedTerm] = parseAll(term, input)
}
