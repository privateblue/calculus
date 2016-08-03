sealed trait Term

object Term {
    case class Variable(index: Int) extends Term
    case class Abstraction(name: String, body: Term) extends Term
    case class Application(fn: Term, arg: Term) extends Term
}
