package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  @JSExport
  def parseAndEvaluateStepped(input: String): scalajs.js.Array[NamedTerm] = {
    Parser.parse(input) match {
      case Parser.Success(namedTerm, _) => {
        val term = NamedTerm.removeNames(namedTerm)
        scalajs.js.Array(CallByValueEvaluator.evaluateStepped(term).map(NamedTerm.restoreNames): _*)
      }
      case Parser.NoSuccess(msg, _) => {
        throw new scalajs.js.JavaScriptException(msg)
      }
    }
  }
}
