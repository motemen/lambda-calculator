import org.scalatest._

class TermSpec extends FlatSpec with Matchers {
  "VariableTerm" should "stringify to term name" in {
    VariableTerm("xyz").toString shouldBe "xyz"
  }

  "AbstractionTerm" should "stringify to λx.y" in {
    AbstractionTerm(VariableTerm("x"), VariableTerm("y")).toString shouldBe "λx.y"
  }

  it should "stringify to λx.(y z)" in {
    AbstractionTerm(VariableTerm("x"), ApplicationTerm(VariableTerm("y"), VariableTerm("z"))).toString shouldBe "λx.(y z)"
  }

  "ApplicationTerm" should "stringify to f a" in {
    ApplicationTerm(VariableTerm("f"), VariableTerm("a")).toString shouldBe "f a"
  }

  it should "stringify to (λa.b) (g x)" in {
    ApplicationTerm(AbstractionTerm(VariableTerm("a"), VariableTerm("b")), (ApplicationTerm(VariableTerm("g"), VariableTerm("x")))).toString shouldBe "(λa.b) (g x)"
  }
}
