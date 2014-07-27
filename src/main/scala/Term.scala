package net.tokyoenvious.lambdacalc

object NamedTerm {
  type Context = List[String]

  def removeNames(term: NamedTerm, context: Map[String,Int] = Map.empty): Term = {
    term match {
      case NamedVar(name) => {
        Var(context.get(name).get, name)
      }

      case NamedAbs(NamedVar(name), body) => {
        Abs(NamedTerm.removeNames(body, context.mapValues(_+1) + (name -> 0)), name)
      }

      case NamedApp(fun, arg) => {
        App(NamedTerm.removeNames(fun, context), NamedTerm.removeNames(arg, context))
      }
    }
  }

  def restoreNames(term: Term): NamedTerm = {
    term match {
      case Var(i, name) => {
        NamedVar(name)
      }
      case Abs(body, argName) => {
        NamedAbs(NamedVar(argName), restoreNames(body))
      }
      case App(fun, arg) => {
        NamedApp(restoreNames(fun), restoreNames(arg))
      }
    }
  }
}

sealed trait NamedTerm extends Displayable {
  override def toString = toDisplay().toString
}

case class NamedVar(name: String) extends NamedTerm {
  def buildDisplay = { _ => Literal(name) }
}

case class NamedAbs(arg: NamedVar, body: NamedTerm) extends NamedTerm {
  def buildDisplay = {
    recurse => Concat(Seq(
      Literal("λ"),
      recurse(arg, 0),
      Literal("."),
      recurse(body, 1)
    ))
  }
}

case class NamedApp(fun: NamedTerm, arg: NamedTerm) extends NamedTerm {
  def buildDisplay = {
    recurse => {
      val funDecorator = fun match {
        case NamedAbs(_, _) => Parenthesized
        case _              => identity[Display] _
      }
      val argDecorator = arg match {
        case NamedVar(_) => identity[Display] _
        case _           => Parenthesized
      }

      Concat(Seq(
        funDecorator(recurse(fun, 0)),
        Literal(" "),
        argDecorator(recurse(arg, 1))
      ))
    }
  }
}

sealed trait Term

case class Var(index: Int, name: String = "") extends Term {
  override def toString = s"$index"
}

case class Abs(body: Term, argName: String = "") extends Term {
  override def toString = s"λ$body"
}

case class App(fun: Term, arg: Term) extends Term {
  override def toString = {
    def inner(term: Term): String = {
      term match {
        case Var(_, _) => term.toString
        case _ => s"($term)"
      }
    }

    s"${inner(fun)} ${inner(arg)}"
  }
}

sealed trait Display

case class Literal(d: String) extends Display {
  override def toString = d
}
case class Concat(dd: Seq[Display]) extends Display {
  override def toString = dd.mkString("")
}
case class Parenthesized(d: Display) extends Display {
  override def toString = s"($d)"
}
case class Focused(d: Display) extends Display {
  override def toString = s"*$d*"
}

trait Displayable { self =>
  def buildDisplay: ((Displayable, Int) => Display) => Display

  def toDisplay(focus: Seq[Int] = Seq()): Display = {
    val sp = buildDisplay((s: Displayable, i: Int) => s.toDisplay(subFocus(focus, i)))
    focus match {
      case Seq(0) => Focused(sp)
      case _      => sp
    }
  }

  private def subFocus(focus: Seq[Int], index: Int): Seq[Int] = {
    focus match {
      case `index` :: tail => tail
      case _ => Seq()
    }
  }
}