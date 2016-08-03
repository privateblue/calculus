import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

import cats.data.Xor

object Parser extends StdTokenParsers with PackratParsers {
    sealed trait ParsedTerm
    object ParsedTerm {
        case class Variable(name: String) extends ParsedTerm
        case class Abstraction(name: String, body: ParsedTerm) extends ParsedTerm
        case class Application(fn: ParsedTerm, arg: ParsedTerm) extends ParsedTerm
    }

    def canonical(named: ParsedTerm, names: List[String] = List.empty[String]): Xor[String, Term] = named match {
        case ParsedTerm.Variable(name) =>
            if (names.indexOf(name) == -1) Xor.left(s"$name not found")
            else Xor.right(Term.Variable(names.indexOf(name)))
        case ParsedTerm.Abstraction(name, body) =>
            canonical(body, name::names) match {
                case Xor.Right(b) => Xor.right(Term.Abstraction(name, b))
                case l@Xor.Left(_) => l
            }
        case ParsedTerm.Application(fn, arg) =>
            (canonical(fn, names), canonical(arg, names)) match {
                case (l@Xor.Left(_), _) => l
                case (_, l@Xor.Left(_)) => l
                case (Xor.Right(f), Xor.Right(a)) => Xor.right(Term.Application(f, a))
            }
    }

    type Tokens = StdLexical

    class Lexer extends StdLexical {
        override def letter = elem("letter", c => c.isLetter && c != 'λ')
    }

    val lexical = new Lexer

    lexical.delimiters ++= Seq("\\", "λ", ".", "(", ")")

    def parse(str: String): Xor[String, Term] = {
        val tokens = new lexical.Scanner(str)
        phrase(term)(tokens) match {
            case Success(parsed, _) => canonical(parsed)
            case NoSuccess(err, _) => Xor.left(err)
        }
    }

    lazy val term: PackratParser[ParsedTerm] =
        abstraction | application | variable | parens

    lazy val abstraction: PackratParser[ParsedTerm.Abstraction] =
        ("λ" | "\\") ~> ident ~ "." ~ term ^^ { case name ~ "." ~ body  => ParsedTerm.Abstraction(name, body) }

    lazy val application: PackratParser[ParsedTerm.Application] =
        term ~ term ^^ { case fn ~ arg => ParsedTerm.Application(fn, arg) }

    lazy val variable: PackratParser[ParsedTerm.Variable] = ident ^^ { name => ParsedTerm.Variable(name) }

    lazy val parens: PackratParser[ParsedTerm] = "(" ~> term <~ ")"
}
