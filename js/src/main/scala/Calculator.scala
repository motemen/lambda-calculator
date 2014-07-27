package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  def stringPresentationToJS(sp: Display): scalajs.js.Any = {
    sp match {
      case Literal(s) => s
      case Concat(ss) =>
        scalajs.js.Array(ss.map(stringPresentationToJS): _*)
      case Parenthesized(s) =>
        scalajs.js.Array("(", stringPresentationToJS(s), ")")
      case Focused(s) =>
        scalajs.js.Dynamic.literal("focused" -> true, "content" -> stringPresentationToJS(s))
    }  
  }
  
  @JSExport
  def parseAndEvaluateStepped(input: String): scalajs.js.Any = {
    Parser.parse(input) match {
      case Parser.Success(namedTerm, _) => {
        val stepPresentations = CallByValueEvaluator.evaluateSteppedWithFocus(NamedTerm.removeNames(namedTerm)).map {
          case (term, focusOption) => {
            NamedTerm.restoreNames(term).toDisplay(focusOption.getOrElse(Seq()))
          }
        }

        scalajs.js.Array(stepPresentations.map(stringPresentationToJS): _*)
      }
      case Parser.NoSuccess(msg, _) => {
        throw new scalajs.js.JavaScriptException(msg)
      }
    }
  }
}
