import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "The parser" should "parse variables" in {
    val parsed = Parser.parse("x")
    parsed.successful shouldBe true
    parsed.get shouldBe VariableTerm("x")
  }

  it should "parse application" in {
    val parsed = Parser.parse("f a")
    parsed.successful shouldBe true
    parsed.get shouldBe ApplicationTerm(VariableTerm("f"), VariableTerm("a"))
  }

  it should "parse abstraction" in {
    val parsed = Parser.parse("""\x.x""")
    parsed.successful shouldBe true
    parsed.get shouldBe AbstractionTerm(VariableTerm("x"), VariableTerm("x"))
  }

  it should "parse terms in parens" in {
    val parsed = Parser.parse("""(A)""")
    parsed.successful shouldBe true
    parsed.get shouldBe VariableTerm("A")
  }

  it should "parse complicated terms" in {
    val parsed = Parser.parse("""(\xxx.aa bb) (\p.x1 (x2 x3))""")
    parsed.successful shouldBe true
    parsed.get shouldBe ApplicationTerm(
      AbstractionTerm(VariableTerm("xxx"), ApplicationTerm(VariableTerm("aa"), VariableTerm("bb"))),
      AbstractionTerm(VariableTerm("p"), ApplicationTerm(VariableTerm("x1"), ApplicationTerm(VariableTerm("x2"), VariableTerm("x3"))))
    )
  }
}
