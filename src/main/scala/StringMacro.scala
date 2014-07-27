package net.tokyoenvious.lambdacalc

import scala.util.matching.Regex

case class StringMacro(definitions: Map[String,String]) {
  def replace(s: String): String = {
    """\$(\w+)""".r.replaceAllIn(s, { m => Regex.quoteReplacement("(" + replace(definitions(m.group(1))) + ")") })
  }
}

object StringMacro {
  val Prelude = StringMacro(Map(
    "test" -> """\b.\t.\e.b t e""",
    "tru"  -> """\x.\y.x""",
    "fls"  -> """\x.\y.y""",

    "pair" -> """\x.\y.\f.f x y""",
    "fst"  -> """\p.p (\x.\y.x)""",
    "snd"  -> """\p.p (\x.\y.y)""",

    "c0"     -> """\s.\z.z""",
    "c1"     -> """\s.\z.s z""",
    "c2"     -> """\s.\z.s (s z)""",
    "c3"     -> """\s.\z.s (s (s z))""",
    "plus"   -> """\n.\m.\s.\z.n s (m s z)""",
    "iszero" -> """\n.n (\x.$fls) $tru""",

    "Y" -> """\f. (\x.f \y. x x y) (\x.f \y. x x y)"""
  ))
}
