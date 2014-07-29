import net.tokyoenvious.lambdacalc._
import org.scalatest._

class EvaluatorSpec extends FunSuite with Matchers {
  object Evaluator extends Evaluator {
    def step1(term: Term) = ???
  }

  test ("[1 :-> λ0]1 = λ0") {
    val term = Evaluator.substitute(Var(1), 1, Abs(Var(0)))
    term shouldBe Abs(Var(0))
  }

  test ("[2 :-> λ0]λ3 = λλ0") {
    val term = Evaluator.substitute(Abs(Var(3)), 2, Abs(Var(0)))
    term shouldBe Abs(Abs(Var(0)))
  }

  test ("[0 :-> 1]λ1 = λ2") {
    val term = Evaluator.substitute(Abs(Var(1)), 0, Var(1))
    term shouldBe Abs(Var(2))
  }

  test ("(λ.1 0 2)(λ.0) -> 0 (λ.0) 1") {
    Evaluator.resolveApplication(
      App(App(Var(1), Var(0)), Var(2)),
      Abs(Var(0))
    ) shouldBe App(App(Var(0), Abs(Var(0))), Var(1))
  }
}

class CallByValueEvaluatorSpec extends FunSuite with Matchers {
  test ("(λx.x)(λy.y) -> λy.y") {
    CallByValueEvaluator.step1(
      App(Abs(Var(0, "x")), Abs(Var(0, "y")))
    ).map(_._1) shouldBe Some(Abs(Var(0, "y")))
  }

  test ("(λx.λy.y x)(λy.y) -> λy.y λy'. y'") {
    CallByValueEvaluator.step1(
      App(
        Abs(Abs(App(Var(0, "y"), Var(1, "x")))),
        Abs(Var(0, "y"))
      )
    ).map(_._1) shouldBe Some(Abs(
      App(
        Var(0, "y"),
        Abs(Var(0, "y"))
      )
    ))
  }

  test ("(λx.x x)(λx.x x) -> itself") {
    val omega = App(
      Abs(App(Var(0, "x"), Var(0, "x"))),
      Abs(App(Var(0, "x"), Var(0, "x")))
    )

    CallByValueEvaluator.step1(omega).map(_._1) shouldBe Some(omega)
  }

  test ("(λx.λy.λz.(z y) x)(λx.x) -> λy.λz.z y (λx.x)") {
    val namedTerm = Parser.parse("""(\x.\y.\z.(z y) x)(\x.x)""").get

    namedTerm shouldBe NamedApp(
      NamedAbs(NamedVar("x"), NamedAbs(NamedVar("y"), NamedAbs(NamedVar("z"), NamedApp(NamedApp(NamedVar("z"), NamedVar("y")), NamedVar("x"))))),
      NamedAbs(NamedVar("x"), NamedVar("x"))
    )

    val term = NamedTerm.removeNames(namedTerm)

    term shouldBe App(
      Abs(Abs(Abs(App(App(Var(0, "z"), Var(1, "y")), Var(2, "x")), "z"), "y"), "x"),
      Abs(Var(0, "x"), "x")
    )

    CallByValueEvaluator.step1(term).map(_._1) shouldBe Some(
      Abs(Abs(App(App(Var(0, "z"), Var(1, "y")), Abs(Var(0, "x"), "x")), "z"), "y")
    )
  }

  test ("(λx.λy.y (λz.x))(λa.λb.a (b b)) -> λy.y (λz.λa.λb.a (b b))") {
    val namedTerm = Parser.parse("""(\x.\y.y (\z.x))(\a.\b.a (b b))""").get

    namedTerm.toString shouldBe "(λx.λy.y (λz.x)) (λa.λb.a (b b))"

    val term = NamedTerm.removeNames(namedTerm)
    term.toString shouldBe "(λλ0 (λ2)) (λλ1 (0 0))"

    val steppedTerm = CallByValueEvaluator.step1(term).map(_._1)
    steppedTerm.get.toString shouldBe "λ0 (λλλ1 (0 0))"
  }

  test ("(λx.x) ((λy.y) (λz.z)) -> (λx.x) (λz.z)") {
    val term = App(Abs(Var(0, "x")), App(Abs(Var(0, "y")), Abs(Var(0, "z"))))
    val step = CallByValueEvaluator.step1(term).get
    step._1 shouldBe App(Abs(Var(0, "x")), Abs(Var(0, "z")))
    step._2 shouldBe Seq(1, 0)
  }

  test ("(λx.x)((λx.x)(λz.(λx.x)z))") {
    val term = NamedTerm.removeNames(Parser.parse("(λx.x)((λx.x)(λz.(λx.x)z))").get)

    val (term2, focus) = CallByValueEvaluator.step1(term).get
    term2 shouldBe App(Abs(Var(0,"x"), "x"), Abs(App(Abs(Var(0, "x"), "x"), Var(0, "z")), "z"))
    focus shouldBe Seq(1, 0)
  }
}

class CallByNameEvaluatorSpec extends FunSuite with Matchers {
  test ("(λx.x)((λx.x)(λz.(λx.x)z))") {
    val term = NamedTerm.removeNames(Parser.parse("(λx.x)((λx.x)(λz.(λx.x)z))").get)

    val (term2, focus) = CallByNameEvaluator.step1(term).get
    term2 shouldBe App(Abs(Var(0,"x"), "x"), Abs(App(Abs(Var(0, "x"), "x"), Var(0, "z")), "z"))
    focus shouldBe Seq(0)
  }
}