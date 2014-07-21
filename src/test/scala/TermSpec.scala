import org.scalatest._

class TermSpec extends FlatSpec with Matchers {
  "VariableTerm" should "stringify to term name" in {
    Variable("xyz").toString shouldBe "xyz"
  }

  "AbstractionTerm" should "stringify to λx.y" in {
    Abstraction(Variable("x"), Variable("y")).toString shouldBe "λx.y"
  }

  it should "stringify to λx.(y z)" in {
    Abstraction(Variable("x"), Application(Variable("y"), Variable("z"))).toString shouldBe "λx.(y z)"
  }

  "ApplicationTerm" should "stringify to f a" in {
    Application(Variable("f"), Variable("a")).toString shouldBe "f a"
  }

  it should "stringify to (λa.b) (g x)" in {
    Application(Abstraction(Variable("a"), Variable("b")), (Application(Variable("g"), Variable("x")))).toString shouldBe "(λa.b) (g x)"
  }
}
