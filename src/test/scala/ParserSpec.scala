import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "The parser" should "parse variables" in {
    val parsed = Parser.parse("x")
    parsed.successful shouldBe true
    parsed.get shouldBe Variable("x")
  }

  it should "parse application" in {
    val parsed = Parser.parse("f a")
    parsed.successful shouldBe true
    parsed.get shouldBe Application(Variable("f"), Variable("a"))
  }

  it should "parse abstraction" in {
    val parsed = Parser.parse("""\x.x""")
    parsed.successful shouldBe true
    parsed.get shouldBe Abstraction(Variable("x"), Variable("x"))
  }

  it should "parse terms in parens" in {
    val parsed = Parser.parse("""(A)""")
    parsed.successful shouldBe true
    parsed.get shouldBe Variable("A")
  }

  it should "parse complicated terms" in {
    val parsed = Parser.parse("""(\xxx.aa bb) (\p.x1 (x2 x3))""")
    parsed.successful shouldBe true
    parsed.get shouldBe Application(
      Abstraction(Variable("xxx"), Application(Variable("aa"), Variable("bb"))),
      Abstraction(Variable("p"), Application(Variable("x1"), Application(Variable("x2"), Variable("x3"))))
    )
  }
}
