import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

import scala.util.Either

trait Parser extends StdTokenParsers with PackratParsers {
    self: AST =>

    sealed trait ParsedTerm
    object ParsedTerm {
        case class Variable(name: String) extends ParsedTerm
        case class Abstraction(name: String, body: ParsedTerm) extends ParsedTerm
        case class Application(fn: ParsedTerm, arg: ParsedTerm) extends ParsedTerm
    }

    def canonical(named: ParsedTerm, names: List[String] = List.empty[String]): Either[String, Term] = named match {
        case ParsedTerm.Variable(name) =>
            if (names.indexOf(name) == -1) Left(s"$name not found")
            else Right(Term.Variable(names.indexOf(name)))
        case ParsedTerm.Abstraction(name, body) =>
            canonical(body, name::names) match {
                case Right(b) => Right(Term.Abstraction(name, b))
                case l@Left(_) => l
            }
        case ParsedTerm.Application(fn, arg) =>
            (canonical(fn, names), canonical(arg, names)) match {
                case (l@Left(_), _) => l
                case (_, l@Left(_)) => l
                case (Right(f), Right(a)) => Right(Term.Application(f, a))
            }
    }

    type Tokens = StdLexical

    class Lexer extends StdLexical {
        override def letter = elem("letter", c => c.isLetter && c != 'λ')
    }

    val lexical = new Lexer

    lexical.delimiters ++= Seq("(", ")")
    lexical.reserved += ("fun")

    def parse(str: String): Either[String, Term] = {
        val tokens = new lexical.Scanner(str)
        phrase(term)(tokens) match {
            case Success(parsed, _) => canonical(parsed)
            case NoSuccess(err, _) => Left(err)
        }
    }

    lazy val term: PackratParser[ParsedTerm] =
        abstraction | application | variable | parens

    lazy val abstraction: PackratParser[ParsedTerm.Abstraction] =
        "fun" ~> varInParens ~ term ^^ { case ParsedTerm.Variable(name) ~ body  => ParsedTerm.Abstraction(name, body) }

    lazy val application: PackratParser[ParsedTerm.Application] =
        term ~ term ^^ { case fn ~ arg => ParsedTerm.Application(fn, arg) }

    lazy val variable: PackratParser[ParsedTerm.Variable] = ident ^^ { name => ParsedTerm.Variable(name) }

    lazy val parens: PackratParser[ParsedTerm] = "(" ~> term <~ ")"

    lazy val varInParens: PackratParser[ParsedTerm.Variable] = "(" ~> variable <~ ")"
}
