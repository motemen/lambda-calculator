import org.scalatest._

class EvaluatorSpec extends FlatSpec with Matchers {
  val idX = Abstraction(Variable(0, "x"))

  "substitute" should "[1 :-> λ0]1 = λ0" in {
    val term = Evaluator.substitute(Variable(1, ""), 1, Abstraction(Variable(0, "")))
    term shouldBe Abstraction(Variable(0, ""))
  }

  it should "[2 :-> λ0]λ3 = λλ0" in {
    val term = Evaluator.substitute(Abstraction(Variable(3, "")), 2, Abstraction(Variable(0, "")))
    term shouldBe Abstraction(Abstraction(Variable(0, "")))
  }

  it should "[0 :-> 1]λ1 = λ2" in {
    val term = Evaluator.substitute(Abstraction(Variable(1, "")), 0, Variable(1, ""))
    term shouldBe Abstraction(Variable(2, ""))
  }

  "step1" should "evaluate (λx.x)(λy.y) to λy.y" in {
    Evaluator.step1(
      Application(Abstraction(Variable(0, "x")), Abstraction(Variable(0, "y")))
    ) shouldBe Abstraction(Variable(0, "y"))
  }

  it should "evaluate (λx.λy.y x)(λy.y) to λy.y λy'. y'" in {
    Evaluator.step1(
      Application(
        Abstraction(Abstraction(Application(Variable(0, "y"), Variable(1, "x")))),
        Abstraction(Variable(0, "y"))
      )
    ) shouldBe Abstraction(
      Application(
        Variable(0, "y"),
        Abstraction(Variable(0, "y"))
      )
    )
  }

  it should "evaluate (λx.x x)(λx.x x) to itself" in {
    val omega = Application(
      Abstraction(Application(Variable(0, "x"), Variable(0, "x"))),
      Abstraction(Application(Variable(0, "x"), Variable(0, "x")))
    )

    Evaluator.step1(omega) shouldBe omega
  }

  it should "evaluate (λx.λy.λz.(z y) x)(λx.x) to λy.λz.z y (λx.x)" in {
    val namedTerm = Parser.parse("""(\x.\y.\z.(z y) x)(\x.x)""").get

    namedTerm shouldBe NamedApplication(
      NamedAbstraction(NamedVariable("x"), NamedAbstraction(NamedVariable("y"), NamedAbstraction(NamedVariable("z"), NamedApplication(NamedApplication(NamedVariable("z"), NamedVariable("y")), NamedVariable("x"))))),
      NamedAbstraction(NamedVariable("x"), NamedVariable("x"))
    )

    val term = NamedTerm.removeNames(namedTerm)

    term shouldBe Application(
      Abstraction(Abstraction(Abstraction(Application(Application(Variable(0, "z"), Variable(1, "y")), Variable(2, "x"))))),
      idX
    )

    Evaluator.step1(term) shouldBe Abstraction(Abstraction(Application(Application(Variable(0, "z"), Variable(1, "y")), Abstraction(Variable(0, "x")))))
  }

  it should "evaluate (λx.λy.y (λz.x))(λa.λb.a (b b)) to λy.y (λz.λa.λb.a (b b))" in {
    val namedTerm = Parser.parse("""(\x.\y.y (\z.x))(\a.\b.a (b b))""").get

    namedTerm.toString shouldBe "(λx.λy.y (λz.x)) (λa.λb.a (b b))"

    val term = NamedTerm.removeNames(namedTerm)
    term.toString shouldBe "(λλ0 (λ2)) (λλ1 (0 0))"

    val steppedTerm = Evaluator.step1(term)
    steppedTerm.toString shouldBe "λ0 (λλλ1 (0 0))"
  }
}
