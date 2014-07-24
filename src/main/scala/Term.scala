package net.tokyoenvious.lambdacalc

object NamedTerm {
  type Context = List[String]

  def removeNames(term: NamedTerm, context: Map[String,Int] = Map.empty): Term = {
    term match {
      case NamedVariable(name) => {
        Variable(context.get(name).get, name)
      }

      case NamedAbstraction(NamedVariable(name), body) => {
        Abstraction(NamedTerm.removeNames(body, context.mapValues(_+1) + (name -> 0)), name)
      }

      case NamedApplication(fun, arg) => {
        Application(NamedTerm.removeNames(fun, context), NamedTerm.removeNames(arg, context))
      }
    }
  }

  def restoreNames(term: Term): NamedTerm = {
    term match {
      case Variable(i, name) => {
        NamedVariable(name)
      }
      case Abstraction(body, argName) => {
        NamedAbstraction(NamedVariable(argName), restoreNames(body))
      }
      case Application(fun, arg) => {
        NamedApplication(restoreNames(fun), restoreNames(arg))
      }
    }
  }
}

sealed trait NamedTerm

case class NamedVariable(name: String) extends NamedTerm {
  override def toString = name
}

case class NamedAbstraction(arg: NamedVariable, body: NamedTerm) extends NamedTerm {
  override def toString = s"λ$arg.$body"
}

case class NamedApplication(fun: NamedTerm, arg: NamedTerm) extends NamedTerm {
  override def toString = {
    def inner(term: NamedTerm): String = {
      term match {
        case NamedVariable(_) => term.toString
        case _ => s"($term)"
      }
    }

    s"${inner(fun)} ${inner(arg)}"
  }
}

sealed trait Term

case class Variable(index: Int, name: String) extends Term {
  override def toString = s"$index"
}

case class Abstraction(body: Term, argName: String = "") extends Term {
  override def toString = s"λ$body"
}

case class Application(fun: Term, arg: Term) extends Term {
  override def toString = {
    def inner(term: Term): String = {
      term match {
        case Variable(_, _) => term.toString
        case _ => s"($term)"
      }
    }

    s"${inner(fun)} ${inner(arg)}"
  }
}
