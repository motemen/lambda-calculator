object Evaluator {
  def step1(term: Term): Term = {
    /**
     * Substitutes the variable with `index` to `newTerm`
     */
    def substitute(term: Term, index: Int, newTerm: Term): Term = {
      term match {
        case Variable(i, _) if i == index => newTerm
        case Variable(_, _)               => term
        case Application(f, a) => Application(substitute(f, index, newTerm), substitute(a, index, newTerm))
        case Abstraction(b)    => Abstraction(substitute(b, index + 1, newTerm))
      }
    }

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
