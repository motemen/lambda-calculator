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

    "pair" -> """\f.\s.\b.b f s""",
    "fst"  -> """\p.p $tru""",
    "snd"  -> """\p.p $fls""",

    "c0"    -> """\s.\z.z""",
    "c1"    -> """\s.\z.s z""",
    "c2"    -> """\s.\z.s (s z)""",
    "c3"    -> """\s.\z.s (s (s z))""",
    "plus"  -> """\m.\n.\s.\z.m s (n s z)""",
    "iszro" -> """\m.m (\x.$fls) $tru""",

    "fix" -> """\f.(\x.f (\y.x x y)) (\x.f (\y.x x y))""",

    "omega" -> """(\x.x x)(\x.x x)""",

    "zz" -> """$pair $c0 $c0""",
    "ss" -> """\p.$pair ($snd p) ($plus $c1 ($snd p))""",
    "prd" -> """\m.$fst (m $ss $zz)"""
  ))
}
