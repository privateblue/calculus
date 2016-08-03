object Evaluator {
    def shift(d: Int, term: Term): Term = {
        def walk(c: Int, t: Term): Term = t match {
            case Term.Variable(index) =>
                if (index >= c) Term.Variable(index + d)
                else Term.Variable(index)
            case Term.Abstraction(name, body) =>
                Term.Abstraction(name, walk(c + 1, body))
            case Term.Application(fn, arg) =>
                Term.Application(walk(c, fn), walk(c, arg))
        }
        walk(0, term)
    }

    def substitute(target: Int, subs: Term, term: Term): Term = {
        def walk(c: Int, t: Term): Term = t match {
            case Term.Variable(index) =>
                if (index == target + c) shift(c, subs)
                else Term.Variable(index)
            case Term.Abstraction(name, body) =>
                Term.Abstraction(name, walk(c + 1, body))
            case Term.Application(fn, arg) =>
                Term.Application(walk(c, fn), walk(c, arg))
        }
        walk(0, term)
    }

    def reduce(subs: Term, term: Term): Term =
        shift(-1, substitute(0, shift(1, subs), term))

    def isValue(term: Term): Boolean = term match {
        case Term.Abstraction(_, _) => true
        case _ => false
    }

    def step(term: Term): Option[Term] = term match {
        case Term.Application(Term.Abstraction(_, body), arg) if isValue(arg) =>
            Some(reduce(arg, body))
        case Term.Application(fn, arg) if isValue(fn) =>
            step(arg).map(Term.Application(fn, _))
        case Term.Application(fn, arg) =>
            step(fn).map(Term.Application(_, arg))
        case _ =>
            None
    }

    def eval(term: Term): Term =
        step(term).map(eval).getOrElse(term)
}
