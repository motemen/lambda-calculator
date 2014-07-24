package net.tokyoenvious.lambdacalc

trait Evaluator {
  def shift(term: Term, offset: Int, cutoff: Int): Term = {
    term match {
      case Variable(i, n) if i >= cutoff => Variable(i + offset, n)
      case Variable(_, _)                => term
      case Application(f, a) => Application(shift(f, offset, cutoff), shift(a, offset, cutoff))
      case Abstraction(b, n) => Abstraction(shift(b, offset + 1, cutoff + 1), n)
    }
  }

  /**
   * Substitutes the variable with `index` to `newTerm`
   */
  def substitute(term: Term, index: Int, newTerm: Term): Term = {
    term match {
      case Variable(i, _) if i == index => newTerm
      case Variable(_, _)               => term
      case Application(f, a) => Application(substitute(f, index, newTerm), substitute(a, index, newTerm))
      case Abstraction(b, n) => Abstraction(substitute(b, index + 1, shift(newTerm, 1, 0)), n)
    }
  }

  def isValue(term: Term): Boolean = {
    term match {
      case Abstraction(_, _) => true
      case _                 => false
    }
  }

  def resolveApplication(body: Term, arg: Term): Term = {
    shift(substitute(body, 0, arg), -1, 0)
  }

  def evaluateStepped(term: Term): Seq[Term] = {
    def next(term: Term = term, steps: Seq[Term] = Seq(term)): Seq[Term] = {
      step1(term) match {
        case None        => steps
        case Some(term_) => next(term_, steps :+ term_)
      }
    }

    next()
  }

  def step1(term: Term): Option[Term]
}

object CallByValueEvaluator extends Evaluator {
  def step1(term: Term): Option[Term] = {
    term match {
      // E-AppAbs
      case Application(Abstraction(body, _), arg) if isValue(arg) => {
        Some(resolveApplication(body, arg))
      }

      // E-App2
      case Application(fun, arg) if isValue(fun) => {
        step1(arg).map(arg_ => Application(fun, arg_))
      }

      // E-App1
      case Application(fun, arg) => {
        step1(fun).map(fun_ => Application(fun_, arg))
      }

      case _ => None
    }
  }
}
