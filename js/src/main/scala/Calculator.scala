package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  case class JSTerm(namedTerm: NamedTerm, focus: Seq[Int] = Seq()) {
    @JSExport
    def toDisplay() = displayAsJs(namedTerm.toDisplay(focus))
  }

  def displayAsJs(d: Display): scalajs.js.Any = {
    d match {
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
  def parse(input: String): JSTerm = {
    val expandedInput = StringMacro.Prelude.replace(input)

    Parser.parse(expandedInput) match {
      case Parser.Success(namedTerm, _) => JSTerm(namedTerm)
      case Parser.NoSuccess(msg, _) => throw new scalajs.js.JavaScriptException(msg)
    }
  }

  @JSExport
  def evaluate1(t: JSTerm): scalajs.js.Any = {
    try {
      CallByValueEvaluator.step1(NamedTerm.removeNames(t.namedTerm)) match {
        case Some((nextTerm, focus)) => {
          val nextNamedTerm = NamedTerm.restoreNames(nextTerm)
          scalajs.js.Dynamic.literal(
            "prev" -> JSTerm(t.namedTerm, focus).asInstanceOf[scalajs.js.Any],
            "next" -> JSTerm(nextNamedTerm).asInstanceOf[scalajs.js.Any]
          )
        }

        case None => null
      }
    } catch {
      case e: NamedTerm.UnboundVariableFound => throw new scalajs.js.JavaScriptException(e.toString)
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
