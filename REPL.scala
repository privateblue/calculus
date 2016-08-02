import cats._
import cats.data.State
import scala.io.StdIn

object REPL {
    def main(args: Array[String]): Unit = loop.run((List(), 1)).value

    type Env = (List[(String, Term)], Int)

    def loop: State[Env, Unit] = {
        val input = StdIn.readLine("let> ")
        if (input == ":q") State.pure(())
        else for {
            msg <- process(input)
            _ = println(msg)
            _ <- loop
        } yield ()
    }

    def process(input: String): State[Env, String] = State { case (env, c) =>
        val result = for {
            term <- Parser.parse(input)
            eval <- Evaluator.eval(term, env)
        } yield eval
        result.fold(((env, c), _), { case (term, _) =>
            val name = s"res$c"
            val s = Printer.print(term)
            (((name -> term) :: env, c + 1), s"$name: $s")
        })
    }
}
