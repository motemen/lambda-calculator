import org.scalatest._

class TermSpec extends FlatSpec with Matchers {
  "VariableTerm" should "stringify to term name" in {
    NamedVariable("xyz").toString shouldBe "xyz"
  }

  "AbstractionTerm" should "stringify to λx.y" in {
    NamedAbstraction(NamedVariable("x"), NamedVariable("y")).toString shouldBe "λx.y"
  }

  it should "stringify to λx.y z" in {
    NamedAbstraction(NamedVariable("x"), NamedApplication(NamedVariable("y"), NamedVariable("z"))).toString shouldBe "λx.y z"
  }

  "ApplicationTerm" should "stringify to f a" in {
    NamedApplication(NamedVariable("f"), NamedVariable("a")).toString shouldBe "f a"
  }

  it should "stringify to (λa.b) (g x)" in {
    NamedApplication(NamedAbstraction(NamedVariable("a"), NamedVariable("b")), NamedApplication(NamedVariable("g"), NamedVariable("x"))).toString shouldBe "(λa.b) (g x)"
  }

  "removeNames" should "work on λx.λy.(x y)" in {
    NamedTerm.removeNames(
      NamedAbstraction(NamedVariable("x"), NamedAbstraction(NamedVariable("y"), NamedApplication(NamedVariable("x"), NamedVariable("y"))))
    ) shouldBe Abstraction(Abstraction(Application(Variable(1, "x"), Variable(0, "y"))))
  }
}
