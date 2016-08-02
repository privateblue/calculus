sealed trait Term

object Term {
    case class Variable(name: String) extends Term
    case class Abstraction(name: String, body: Term) extends Term
    case class Application(fn: Term, arg: Term) extends Term
    case class Let(name: String, bound: Term, body: Term) extends Term
}
