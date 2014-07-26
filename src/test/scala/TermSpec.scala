import net.tokyoenvious.lambdacalc._
import org.scalatest._

class TermSpec extends FlatSpec with Matchers {
  "VariableTerm" should "stringify to term name" in {
    NamedVar("xyz").toString shouldBe "xyz"
  }

  "AbstractionTerm" should "stringify to λx.y" in {
    NamedAbs(NamedVar("x"), NamedVar("y")).toString shouldBe "λx.y"
  }

  it should "stringify to λx.y z" in {
    NamedAbs(NamedVar("x"), NamedApp(NamedVar("y"), NamedVar("z"))).toString shouldBe "λx.y z"
  }

  "ApplicationTerm" should "stringify to f a" in {
    NamedApp(NamedVar("f"), NamedVar("a")).toString shouldBe "f a"
  }

  it should "stringify to (λa.b) (g x)" in {
    NamedApp(NamedAbs(NamedVar("a"), NamedVar("b")), NamedApp(NamedVar("g"), NamedVar("x"))).toString shouldBe "(λa.b) (g x)"
  }

  "removeNames" should "work on λx.λy.(x y)" in {
    NamedTerm.removeNames(
      NamedAbs(NamedVar("x"), NamedAbs(NamedVar("y"), NamedApp(NamedVar("x"), NamedVar("y"))))
    ) shouldBe Abs(Abs(App(Var(1, "x"), Var(0, "y")), "y"), "x")
  }

  "remove/restoreNames" should "work on λx.λy.(x y)" in {
    val namedTerm = NamedAbs(NamedVar("x"), NamedAbs(NamedVar("y"), NamedApp(NamedVar("x"), NamedVar("y"))))

    val term = NamedTerm.removeNames(namedTerm)

    NamedTerm.restoreNames(term) shouldBe namedTerm
  }
}
