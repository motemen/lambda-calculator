package net.tokyoenvious.lambdacalc

trait Evaluator {
  def shift(term: Term, offset: Int, cutoff: Int): Term = {
    term match {
      case Var(i, n) if i >= cutoff => Var(i + offset, n)
      case Var(_, _)                => term
      case App(f, a) => App(shift(f, offset, cutoff), shift(a, offset, cutoff))
      case Abs(b, n) => Abs(shift(b, offset + 1, cutoff + 1), n)
    }
  }

  /**
   * Substitutes the variable with `index` to `newTerm`
   */
  def substitute(term: Term, index: Int, newTerm: Term): Term = {
    term match {
      case Var(i, _) if i == index => newTerm
      case Var(_, _)               => term
      case App(f, a) => App(substitute(f, index, newTerm), substitute(a, index, newTerm))
      case Abs(b, n) => Abs(substitute(b, index + 1, shift(newTerm, 1, 0)), n)
    }
  }

  def isValue(term: Term): Boolean = {
    term match {
      case Abs(_, _) => true
      case _         => false
    }
  }

  def resolveApplication(body: Term, arg: Term): Term = {
    shift(substitute(body, 0, arg), -1, 0)
  }

  type EvaluationStep = List[(Term,Option[Seq[Int]])]

  def evaluateSteppedWithFocus(term: Term): EvaluationStep = {
    def next(term: Term, step: EvaluationStep): EvaluationStep = {
      step1(term) match {
        case None => (term, None) :: step
        case Some((term_, focus)) => next(term_, (term, Some(focus)) :: step)
      }
    }

    next(term, List()).reverse
  }

  def evaluateStepped(term: Term): List[Term] = {
    evaluateSteppedWithFocus(term).map(_._1)
  }

  def step1(term: Term): Option[(Term,Seq[Int])]
}

object CallByValueEvaluator extends Evaluator {
  def step1(term: Term): Option[(Term,Seq[Int])] = {
    term match {
      // E-AppAbs
      case App(Abs(body, _), arg) if isValue(arg) => {
        Some((resolveApplication(body, arg), Seq(0)))
      }

      // E-App2
      case App(fun, arg) if isValue(fun) => {
        step1(arg).map { case (arg_, focus) => ( App(fun, arg_), focus :+ 1 ) }
      }

      // E-App1
      case App(fun, arg) => {
        step1(fun).map { case (fun_, focus) => ( App(fun_, arg), focus :+ 0 ) }
      }

      case _ => None
    }
  }
}
