object REPL {
    def main(args: Array[String]): Unit = loop

    def loop: Unit = {
        val input = readLine("λ> ")
        if (input == ":q") ()
        else {
            val msg = process(input)
            println(msg)
            loop
        }
    }

    def process(input: String): String =
        Parser.parse(input).map(Evaluator.eval).map(Printer.print).merge
}
