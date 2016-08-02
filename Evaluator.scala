import cats.data.Xor

object Evaluator {
    type Env = List[(String, Term)]

    def eval(term: Term): Xor[String, Term] =
        eval(term, List()).map(_._1)

    def eval(term: Term, bindings: Env): Xor[String, (Term, Env)] = term match {
        case Term.Let(name, bound, body) => eval(body, (name -> bound) :: bindings)

        case Term.Variable(name) => for {
            split <- find(name, bindings)
            (rest, (_, t)::bs) = split
            value <- eval(t, bs)
            (newterm, newbs) = value
        } yield (newterm, rest ++ ((name -> newterm) :: newbs))

        case Term.Abstraction(name, body) => Xor.right((term, bindings))

        case Term.Application(fn, arg) => for {
            value <- eval(fn, bindings)
            (Term.Abstraction(name, body), bs) = value
            result <- eval(body, (name -> arg) :: bs)
        } yield result
    }

    def find(name: String, bindings: Env, prefix: Env = List()): Xor[String, (Env, Env)] = bindings match {
        case Nil => Xor.left(s"$name not found")
        case (n,t)::rest if n == name => Xor.right((prefix, bindings))
        case head::rest => find(name, rest, prefix :+ head)
    }
}
