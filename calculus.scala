import cats.data.Xor

object calculus extends AST with Evaluator with Parser with Printer  {
    def main(args: Array[String]) = while(true) {
        val input = readLine("fun> ")
        handle(input)
    }

    def handle(input: String): Unit =
        if (input == ":q") System.exit(0)
        else parse(input).map(eval) match {
            case Xor.Left(error) => println(s"$error")
            case Xor.Right(term) => println(s"${print(term)}")
        }
}
