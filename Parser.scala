import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

import cats.data.Xor

object Parser extends StdTokenParsers with PackratParsers {
    type Tokens = StdLexical

    val lexical = new StdLexical
    lexical.delimiters ++= Seq("(", ")")
    lexical.reserved += ("fun")

    def parse(str: String): Xor[String, Term] = {
        val tokens = new lexical.Scanner(str)
        phrase(term)(tokens) match {
            case Success(parsed, _) => Xor.right(parsed)
            case NoSuccess(err, _) => Xor.left(err)
        }
    }

    lazy val term: PackratParser[Term] =
        abstraction | application | variable | parens

    lazy val abstraction: PackratParser[Term.Abstraction] =
        "fun" ~> varInParens ~ term ^^ { case Term.Variable(name) ~ body  => Term.Abstraction(name, body) }

    lazy val application: PackratParser[Term.Application] =
        term ~ term ^^ { case fn ~ arg => Term.Application(fn, arg) }

    lazy val variable: PackratParser[Term.Variable] = ident ^^ { name => Term.Variable(name) }

    lazy val parens: PackratParser[Term] = "(" ~> term <~ ")"

    lazy val varInParens: PackratParser[Term.Variable] = "(" ~> variable <~ ")"
}
