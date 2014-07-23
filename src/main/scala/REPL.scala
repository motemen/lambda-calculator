import Parser._

object REPL {
  def main(args: Array[String]) = loop()

  def loop() {
    def steppedEvaluate(term: Term) {
      CallByValueEvaluator.step1(term) match {
        case None       => ()
        case Some(term) => {
          println(s" -> $term")
          steppedEvaluate(term)
        }
      }
    }

    while (true) {
      val line = readLine("λ> ")
      Parser.parse(line) match {
        case Success(namedTerm, _) => {
          println(s"    $namedTerm")

          val term = NamedTerm.removeNames(namedTerm)
          println(s" .. $term")
          steppedEvaluate(term)
        }
        case NoSuccess(msg, _) => println(msg)
      }
    }
  }
}
