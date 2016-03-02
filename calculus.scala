object calculus extends AST with Evaluator with Printer with Parser {
    def main(args: Array[String]) = while(true) {
        val input = readLine("λ> ")
        handle(input)
    }

    def handle(input: String): Unit =
        if (input == ":q") System.exit(0)
        else parse(input) match {
            case Left(error) =>
                println(s"error: $error")
            case Right(term) =>
                println(s"parsed:    ${print(term)}")
                println(s"evaluated: ${print(eval(term))}")
        }
}
