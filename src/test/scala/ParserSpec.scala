import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "The parser" should "parse variables" in {
    val parsed = Parser.parse("x")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedVariable("x")
  }

  it should "parse application" in {
    val parsed = Parser.parse("f a")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedApplication(NamedVariable("f"), NamedVariable("a"))
  }

  it should "parse abstraction" in {
    val parsed = Parser.parse("""\x.x""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedAbstraction(NamedVariable("x"), NamedVariable("x"))
  }

  it should "parse terms in parens" in {
    val parsed = Parser.parse("""(A)""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedVariable("A")
  }

  it should "parse complicated terms" in {
    val parsed = Parser.parse("""(\xxx.aa bb) (\p.x1 (x2 x3))""")
    parsed.successful shouldBe true
    parsed.get shouldBe NamedApplication(
      NamedAbstraction(NamedVariable("xxx"), NamedApplication(NamedVariable("aa"), NamedVariable("bb"))),
      NamedAbstraction(NamedVariable("p"), NamedApplication(NamedVariable("x1"), NamedApplication(NamedVariable("x2"), NamedVariable("x3"))))
    )
  }
}
