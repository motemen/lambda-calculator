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

sealed trait NamedTerm {
  override def toString = toStringPresentation().toString
  def toStringPresentation(focus: Seq[Int] = Seq()): StringPresentation

  protected def subFocus(focus: Seq[Int], index: Int): Seq[Int] = {
    focus match {
      case `index` :: tail => tail
      case _ => Seq()
    }
  }
}

case class NamedVar(name: String) extends NamedTerm {
  override def toStringPresentation(focus: Seq[Int]) =
    focus match {
      case Seq(0) => Focused(AsIs(name))
      case _ => AsIs(name)
    }
}

case class NamedAbs(arg: NamedVar, body: NamedTerm) extends NamedTerm {
  override def toStringPresentation(focus: Seq[Int]) = {
    val sp = Concat(Seq(AsIs("λ"), arg.toStringPresentation(subFocus(focus, 0)), AsIs("."), body.toStringPresentation(subFocus(focus, 1))))
    focus match {
      case Seq(0) => Focused(sp)
      case _ => sp
    }
  }
}

case class NamedApp(fun: NamedTerm, arg: NamedTerm) extends NamedTerm {
  override def toStringPresentation(focus: Seq[Int]) = {
    val funPresentation = fun match {
      case NamedAbs(_,_) => Parenthesized(fun.toStringPresentation(subFocus(focus, 0)))
      case _             => fun.toStringPresentation(subFocus(focus, 0))
    }
    val argPresentation = arg match {
      case NamedVar(_) => arg.toStringPresentation(subFocus(focus, 1))
      case _           => Parenthesized(arg.toStringPresentation(subFocus(focus, 1)))
    }

    val sp = Concat(Seq(funPresentation, AsIs(" "), argPresentation))
    focus match {
      case Seq(0) => Focused(sp)
      case _ => sp
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

sealed trait StringPresentation

case class AsIs(s: String) extends StringPresentation {
  override def toString = s
}
case class Concat(ss: Seq[StringPresentation]) extends StringPresentation {
  override def toString = ss.mkString("")
}
case class Parenthesized(s: StringPresentation) extends StringPresentation {
  override def toString = s"($s)"
}
case class Focused(s: StringPresentation) extends StringPresentation {
  override def toString = s"*$s*"
}
