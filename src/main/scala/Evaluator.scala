object Evaluator {
  def shift(term: Term, offset: Int, cutoff: Int): Term = {
    term match {
      case Variable(i, n) if i >= cutoff => Variable(i + offset, n)
      case Variable(_, _)                => term
      case Application(f, a) => Application(shift(f, offset, cutoff), shift(a, offset, cutoff))
      case Abstraction(b)    => Abstraction(shift(b, offset + 1, cutoff + 1))
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
      case Abstraction(b)    => Abstraction(substitute(b, index + 1, shift(newTerm, 1, 0)))
    }
  }

  def step1(term: Term): Term = {
    term match {
      case Application(fun, arg@Application(_, _)) => {
        Application(fun, step1(arg))
      }

      case Application(Abstraction(body), arg) => {
        substitute(body, 0, arg)
      }

      case Application(fun, arg) => {
        Application(step1(fun), arg)
      }

      case _ => term
    }
  }
}
