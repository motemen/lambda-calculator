import org.scalatest._

class EvaluatorSpec extends FunSuite with Matchers {
  object Evaluator extends Evaluator {
    def step1(term: Term): Option[Term] = ???
  }

  test ("[1 :-> λ0]1 = λ0") {
    val term = Evaluator.substitute(Variable(1, ""), 1, Abstraction(Variable(0, "")))
    term shouldBe Abstraction(Variable(0, ""))
  }

  test ("[2 :-> λ0]λ3 = λλ0") {
    val term = Evaluator.substitute(Abstraction(Variable(3, "")), 2, Abstraction(Variable(0, "")))
    term shouldBe Abstraction(Abstraction(Variable(0, "")))
  }

  test ("[0 :-> 1]λ1 = λ2") {
    val term = Evaluator.substitute(Abstraction(Variable(1, "")), 0, Variable(1, ""))
    term shouldBe Abstraction(Variable(2, ""))
  }

  test ("(λ.1 0 2)(λ.0) -> 0 (λ.0) 1") {
    Evaluator.resolveApplication(
      Application(Application(Variable(1, ""), Variable(0, "")), Variable(2, "")),
      Abstraction(Variable(0, ""))
    ) shouldBe Application(Application(Variable(0, ""), Abstraction(Variable(0, ""))), Variable(1, ""))
  }
}

class CallByValueEvaluatorSpec extends FunSuite with Matchers {
  test ("(λx.x)(λy.y) -> λy.y") {
    CallByValueEvaluator.step1(
      Application(Abstraction(Variable(0, "x")), Abstraction(Variable(0, "y")))
    ) shouldBe Some(Abstraction(Variable(0, "y")))
  }

  test ("(λx.λy.y x)(λy.y) -> λy.y λy'. y'") {
    CallByValueEvaluator.step1(
      Application(
        Abstraction(Abstraction(Application(Variable(0, "y"), Variable(1, "x")))),
        Abstraction(Variable(0, "y"))
      )
    ) shouldBe Some(Abstraction(
      Application(
        Variable(0, "y"),
        Abstraction(Variable(0, "y"))
      )
    ))
  }

  test ("(λx.x x)(λx.x x) -> itself") {
    val omega = Application(
      Abstraction(Application(Variable(0, "x"), Variable(0, "x"))),
      Abstraction(Application(Variable(0, "x"), Variable(0, "x")))
    )

    CallByValueEvaluator.step1(omega) shouldBe Some(omega)
  }

  test ("(λx.λy.λz.(z y) x)(λx.x) -> λy.λz.z y (λx.x)") {
    val namedTerm = Parser.parse("""(\x.\y.\z.(z y) x)(\x.x)""").get

    namedTerm shouldBe NamedApplication(
      NamedAbstraction(NamedVariable("x"), NamedAbstraction(NamedVariable("y"), NamedAbstraction(NamedVariable("z"), NamedApplication(NamedApplication(NamedVariable("z"), NamedVariable("y")), NamedVariable("x"))))),
      NamedAbstraction(NamedVariable("x"), NamedVariable("x"))
    )

    val term = NamedTerm.removeNames(namedTerm)

    term shouldBe Application(
      Abstraction(Abstraction(Abstraction(Application(Application(Variable(0, "z"), Variable(1, "y")), Variable(2, "x"))))),
      Abstraction(Variable(0, "x"))
    )

    CallByValueEvaluator.step1(term) shouldBe Some(
      Abstraction(Abstraction(Application(Application(Variable(0, "z"), Variable(1, "y")), Abstraction(Variable(0, "x")))))
    )
  }

  test ("(λx.λy.y (λz.x))(λa.λb.a (b b)) -> λy.y (λz.λa.λb.a (b b))") {
    val namedTerm = Parser.parse("""(\x.\y.y (\z.x))(\a.\b.a (b b))""").get

    namedTerm.toString shouldBe "(λx.λy.y (λz.x)) (λa.λb.a (b b))"

    val term = NamedTerm.removeNames(namedTerm)
    term.toString shouldBe "(λλ0 (λ2)) (λλ1 (0 0))"

    val steppedTerm = CallByValueEvaluator.step1(term)
    steppedTerm.get.toString shouldBe "λ0 (λλλ1 (0 0))"
  }
}
