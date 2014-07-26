package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  def stringPresentationToJS(sp: StringPresentation): scalajs.js.Any = {
    sp match {
      case AsIs(s) => s
      case Concat(ss) => scalajs.js.Array(ss.map(stringPresentationToJS): _*)
      case Parenthesized(s) => scalajs.js.Array("(", stringPresentationToJS(s), ")")
    }  
  }
  
  @JSExport
  def parseAndEvaluateStepped(input: String): scalajs.js.Array[scalajs.js.Any] = {
    Parser.parse(input) match {
      case Parser.Success(namedTerm, _) => {
        val term = NamedTerm.removeNames(namedTerm)
        val unnamedEvaluationSteps = CallByValueEvaluator.evaluateStepped(term)
        val evaluationSteps = unnamedEvaluationSteps.map(NamedTerm.restoreNames)
        val evaluationStepPresentations = evaluationSteps.map(_.toStringPresentation)
        scalajs.js.Array(evaluationStepPresentations.map(stringPresentationToJS): _*)
      }
      case Parser.NoSuccess(msg, _) => {
        throw new scalajs.js.JavaScriptException(msg)
      }
    }
  }
}
