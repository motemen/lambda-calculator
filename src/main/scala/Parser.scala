import util.parsing.combinator._

/**
 * Syntax:
 * term ::= variable
 *        | term term
 *        | '\' variable '.' term
 */
object Parser extends RegexParsers with PackratParsers {
  lazy val variable: PackratParser[VariableTerm] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ VariableTerm
  lazy val abstraction: PackratParser[AbstractionTerm] = "\\" ~> variable ~ "." ~ term ^^ {
    case (v ~ _ ~ b) => AbstractionTerm(v, b)
  }
  lazy val application: PackratParser[ApplicationTerm] = term ~ term ^^ {
    case (f ~ a) => ApplicationTerm(f, a)
  }
  lazy val parenTerm: PackratParser[Term] = "(" ~> term <~ ")"
  lazy val term: PackratParser[Term] = abstraction ||| application ||| variable ||| parenTerm

  def parse(input: String) = parseAll(term, input)
}

sealed trait Term {
  protected def innerTermToString(term: Term): String = {
    term match {
      case VariableTerm(_) => term.toString
      case _ => s"($term)"
    }
  }
}

case class VariableTerm(name: String) extends Term {
  override def toString = name
}

case class AbstractionTerm(arg: VariableTerm, body: Term) extends Term {
  override def toString = s"λ$arg.${innerTermToString(body)}"
}

case class ApplicationTerm(fun: Term, arg: Term) extends Term {
  override def toString = s"${innerTermToString(fun)} ${innerTermToString(arg)}"
}
