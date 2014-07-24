package net.tokyoenvious.lambdacalc.js

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  @JSExport
  def t1(input: String) = {
    Parser.parse(input) match {
      case Parser.Success(term, _) => term
      case Parser.NoSuccess(msg, _) => {
        throw new scala.scalajs.js.JavaScriptException(msg)
      }
    }
  }

  @JSExport
  def readEval(input: String) = {
    Parser.parse(input) match {
      case Parser.Success(term, _) => {
        CallByValueEvaluator.evaluate(NamedTerm.removeNames(term))
      }
      case Parser.NoSuccess(msg, _) => {
        throw new scala.scalajs.js.JavaScriptException(msg)
      }
    }
  }
}
