import net.tokyoenvious.lambdacalc._
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "The parser" should "parse variables" in {
    val parsed = Parser.parse("x")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedVar("x")
  }

  it should "parse application" in {
    val parsed = Parser.parse("f a")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedApp(NamedVar("f"), NamedVar("a"))
  }

  it should "parse abstraction" in {
    val parsed = Parser.parse("""\x.x""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedAbs(NamedVar("x"), NamedVar("x"))
  }

  it should "parse terms in parens" in {
    val parsed = Parser.parse("""(A)""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedVar("A")
  }

  it should "parse complicated terms" in {
    val parsed = Parser.parse("""(\xxx.aa bb) (\p.x1 (x2 x3))""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedApp(
      NamedAbs(NamedVar("xxx"), NamedApp(NamedVar("aa"), NamedVar("bb"))),
      NamedAbs(NamedVar("p"), NamedApp(NamedVar("x1"), NamedApp(NamedVar("x2"), NamedVar("x3"))))
    )
  }

  it should "application associates in left" in {
    val parsed = Parser.parse("""\x.\y.\z.x y z""")

    parsed.get shouldBe
       NamedAbs(NamedVar("x"),
         NamedAbs(NamedVar("y"),
           NamedAbs(NamedVar("z"),
            NamedApp(NamedApp(NamedVar("x"), NamedVar("y")), NamedVar("z")))))
  }
}
