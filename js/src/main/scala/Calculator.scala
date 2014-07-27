package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  def displayAsJs(sp: Display): scalajs.js.Any = {
    sp match {
      case Literal(s) => s
      case Concat(ss) =>
        scalajs.js.Array(ss.map(displayAsJs): _*)
      case Parenthesized(s) =>
        scalajs.js.Array("(", displayAsJs(s), ")")
      case Focused(s) =>
        scalajs.js.Dynamic.literal("reduced" -> true, "content" -> displayAsJs(s))
    }
  }

  @JSExport
  def parse(input: String): NamedTerm = {
    val expandedInput = StringMacro.Prelude.replace(input)

    Parser.parse(expandedInput) match {
      case Parser.Success(namedTerm, _) => namedTerm
      case Parser.NoSuccess(msg, _) => throw new scalajs.js.JavaScriptException(msg)
    }
  }

  @JSExport
  def evaluationSteps(namedTerm: NamedTerm): scalajs.js.Any = {
    try {
      val stepDisplays = CallByValueEvaluator.evaluateToValue(NamedTerm.removeNames(namedTerm)).map {
        case (term, focusOption) => {
          NamedTerm.restoreNames(term).toDisplay(focusOption.getOrElse(Seq()))
        }
      }

      scalajs.js.Array(stepDisplays.map(displayAsJs): _*)
    } catch {
      case e: NamedTerm.UnboundVariableFound => throw new scalajs.js.JavaScriptException(e.toString)
    }
  }
}
