package net.tokyoenvious.lambdacalc.js

import scalajs.js.annotation._
import net.tokyoenvious.lambdacalc._

@JSExport
object Calculator {
  case class JSTerm(namedTerm: NamedTerm, focus: Seq[Int] = Seq()) {
    @JSExport
    def toDisplay(): scalajs.js.Any = {
      def rec(d: Display): scalajs.js.Any = {
        d match {
          case Literal(s) => s
          case Concat(ss) =>
            scalajs.js.Array(ss.map(rec): _*)
          case Parenthesized(s) =>
            scalajs.js.Array("(", rec(s), ")")
          case Focused(s) =>
            scalajs.js.Dynamic.literal("reduced" -> true, "content" -> rec(s))
        }
      }

      rec(namedTerm.toDisplay(focus))
    }

    @JSExport
    def toSourceString(): String = {
      def rec(nt: NamedTerm): String = {
        nt match {
          case NamedVar(x) => x
          case NamedAbs(NamedVar(x),body) => s"(function ($x) { return ${rec(body)} })"
          case NamedApp(fun, arg) => s"${rec(fun)}(${rec(arg)})"
        }
      }

      rec(namedTerm)
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

  var evaluator: Evaluator = CallByValueEvaluator
  val evaluators = Map(
    "call-by-value" -> CallByValueEvaluator,
    "call-by-name"  -> CallByNameEvaluator
  )

  @JSExport
  def getAvailableStrategies() = scalajs.js.Array("call-by-value", "call-by-name")
  
  @JSExport
  def setStrategy(s: String) {
    evaluator = evaluators(s)
  }

  @JSExport
  def evaluate1(t: JSTerm): scalajs.js.Any = {
    try {
      evaluator.step1(NamedTerm.removeNames(t.namedTerm)) match {
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
}
