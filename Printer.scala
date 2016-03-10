trait Printer {
    self: AST =>

    def print(term: Term, names: List[String] = List()): String = term match {
        case Term.Variable(index) =>
            names(index)
        case Term.Abstraction(name, body) =>
            val fname = freshname(name, names)
            s"fun($fname) ${print(body, fname::names)}"
        case Term.Application(fn@Term.Variable(_), arg) =>
            s"${print(fn, names)}(${print(arg, names)})"
        case Term.Application(fn, arg) =>
            s"(${print(fn, names)})(${print(arg, names)})"
    }

    def freshname(name: String, names: List[String], suffix: Int = 0): String = {
        val fname = if (suffix == 0) name else s"$name$suffix"
        if (names.contains(fname)) freshname(name, names, suffix + 1)
        else fname
    }
}
