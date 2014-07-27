package net.tokyoenvious.lambdacalc

import util.parsing.combinator._

object Parser extends RegexParsers with PackratParsers {
  lazy val variable: PackratParser[NamedVar] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ {
    case name => NamedVar(name)
  }

  lazy val abstraction: PackratParser[NamedAbs] = """[\\λ]""".r  ~> variable ~ "." ~ term ^^ {
    case (v ~ _ ~ b) => NamedAbs(v, b)
  }

  lazy val application: PackratParser[NamedTerm] = rep1(abstraction | variable | parenTerm) ^^ {
    case (t :: ts) => (t /: ts) { NamedApp }
    case _ => ??? // Could not happen
  }

  lazy val parenTerm: PackratParser[NamedTerm] = "(" ~> term <~ ")"

  lazy val term: PackratParser[NamedTerm] =
    abstraction | application | variable | parenTerm |
      failure("""expected '\', 'λ', '(' or variable name""")

  def parse(input: String): ParseResult[NamedTerm] = parseAll(term, input)
}
