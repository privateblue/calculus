object Printer {
    def print(term: Term): String = term match {
        case Term.Variable(name) => name
        case Term.Abstraction(name, body) => s"fun($name) ${print(body)}"
        case Term.Application(fn@Term.Variable(_), arg) => s"${print(fn)}(${print(arg)})"
        case Term.Application(fn, arg) => s"(${print(fn)})(${print(arg)})"
    }
}
