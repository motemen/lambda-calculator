import util.parsing.combinator._

/**
 * Syntax:
 * term ::= variable
 *        | term term
 *        | '\' variable '.' term
 */
object Parser extends RegexParsers with PackratParsers {
  lazy val variable: PackratParser[Variable] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ Variable
  lazy val abstraction: PackratParser[Abstraction] = "\\" ~> variable ~ "." ~ term ^^ {
    case (v ~ _ ~ b) => Abstraction(v, b)
  }
  lazy val application: PackratParser[Application] = term ~ term ^^ {
    case (f ~ a) => Application(f, a)
  }
  lazy val parenTerm: PackratParser[Term] = "(" ~> term <~ ")"
  lazy val term: PackratParser[Term] = abstraction ||| application ||| variable ||| parenTerm

  def parse(input: String) = parseAll(term, input)
}

sealed trait Term {
  protected def innerTermToString(term: Term): String = {
    term match {
      case Variable(_) => term.toString
      case _ => s"($term)"
    }
  }
}

case class Variable(name: String) extends Term {
  override def toString = name
}

case class Abstraction(arg: Variable, body: Term) extends Term {
  override def toString = s"λ$arg.${innerTermToString(body)}"
}

case class Application(fun: Term, arg: Term) extends Term {
  override def toString = s"${innerTermToString(fun)} ${innerTermToString(arg)}"
}
