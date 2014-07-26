import net.tokyoenvious.lambdacalc._
import org.scalatest._

class ParserSpec extends FunSuite with Matchers {
  test ("parse variables") {
    val parsed = Parser.parse("x")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedVar("x")
  }

  test ("parse application") {
    val parsed = Parser.parse("f a")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedApp(NamedVar("f"), NamedVar("a"))
  }

  test ("parse abstraction") {
    val parsed = Parser.parse("""\x.x""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedAbs(NamedVar("x"), NamedVar("x"))
  }

  test ("parse parenthesis") {
    val parsed = Parser.parse("""(A)""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedVar("A")
  }

  test ("parse complicated terms") {
    val parsed = Parser.parse("""(\xxx.aa bb) (\p.x1 (x2 x3))""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedApp(
      NamedAbs(NamedVar("xxx"), NamedApp(NamedVar("aa"), NamedVar("bb"))),
      NamedAbs(NamedVar("p"), NamedApp(NamedVar("x1"), NamedApp(NamedVar("x2"), NamedVar("x3"))))
    )
  }

  test ("application associates leftward") {
    val parsed = Parser.parse("""\x.\y.\z.x y z""")

    parsed.get shouldBe
       NamedAbs(NamedVar("x"),
         NamedAbs(NamedVar("y"),
           NamedAbs(NamedVar("z"),
            NamedApp(NamedApp(NamedVar("x"), NamedVar("y")), NamedVar("z")))))
  }
}
