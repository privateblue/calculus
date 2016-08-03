object Evaluator {
    def eval(term: Term): Term =
        step(term).map(eval).getOrElse(term)

    def step(term: Term): Option[Term] = term match {
        case Term.Application(Term.Abstraction(name, body), arg@Term.Abstraction(_, _)) =>
            Some(substitute(name, body, arg))
        case Term.Application(fn@Term.Abstraction(_, _), arg) =>
            step(arg).map(Term.Application(fn, _))
        case Term.Application(fn, arg) =>
            step(fn).map(Term.Application(_, arg))
        case _ =>
            None
    }

    def substitute(forName: String, inTerm: Term, withTerm: Term): Term = inTerm match {
        case Term.Variable(n) if n == forName =>
            withTerm
        case Term.Abstraction(n, body) if n != forName =>
            Term.Abstraction(n, substitute(forName, body, withTerm))
        case Term.Application(fn, arg) =>
            Term.Application(substitute(forName, fn, withTerm), substitute(forName, arg, withTerm))
        case term =>
            term
    }
}
