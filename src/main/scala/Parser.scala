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

  lazy val application: PackratParser[NamedApplication] = term ~ term ^^ {
    case (f ~ a) => NamedApplication(f, a)
  }

  lazy val parenTerm: PackratParser[NamedTerm] = "(" ~> term <~ ")"

  lazy val term: PackratParser[NamedTerm] =
    abstraction ||| application ||| variable ||| parenTerm |
      failure("expected '\\', '(' or variable")

  def parse(input: String): ParseResult[NamedTerm] = parseAll(term, input)
}
