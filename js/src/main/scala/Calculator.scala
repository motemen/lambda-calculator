package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  def stringPresentationToJS(sp: StringPresentation): scalajs.js.Any = {
    sp match {
      case AsIs(s) => s
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
        val term = NamedTerm.removeNames(namedTerm)
        val unnamedEvaluationSteps = CallByValueEvaluator.evaluateSteppedWithFocus(term)

        val focuses = unnamedEvaluationSteps.map(_._2)

        val namedTerms = unnamedEvaluationSteps.map(_._1).map(NamedTerm.restoreNames)
        val namedTermPresentations =
          (namedTerms zip focuses).map { case (nt,f) => nt.toStringPresentation(f.getOrElse(Seq())) }

        scalajs.js.Array(namedTermPresentations.map(stringPresentationToJS): _*)
      }
      case Parser.NoSuccess(msg, _) => {
        throw new scalajs.js.JavaScriptException(msg)
      }
    }
  }
}
