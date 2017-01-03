package us.zuercher.divvy

import java.io.Reader
import scala.util.parsing.combinator._
import scala.util.parsing.input.Position

case class ParserFailure(msg: String, val pos: Position) extends Exception(msg)

/**
 * Parser for:
 *
 *   participants: {list-of-people}
 *   {name} spent {amount} [for {list-of-people}] on {description}
 *
 * Names are simple alphanumeric strings with no punctuation or spaces.
 * Amounts are optionally prefixed with a dollar sign ($) and can be whole
 *   numbers or decimal numbers with exactly two decimal places. Leading
 *   zeroes may be omitted. Examples: $100.45, 100.45, 100, $0.99, 0.99, .99
 * A list of people is 1 or more names as an English language list. Examples:
 *   alice
 *   alice and bob
 *   alice, bob and carol
 *   alice, bob, carol, and dave
 *
 * If participants are not explicitly listed, they are assumed to the union
 * of all creditors and debtors. Non-participants are assumed to be paid in
 * full for their credits.
 */
class Parser extends RegexParsers {
  def name: Parser[String] = """[A-Za-z]+""".r ^^ { _.toString }

  def nameSep: Parser[String] = ("," ~> "and") | "," | "and"

  def names: Parser[List[String]] = rep1sep(name, nameSep)

  def amount: Parser[String] =
      """\$?([0-9]+(?:\.[0-9]{2})?)""".r ^^ { _.toString }

  def spentAmount: Parser[Amount] = "spent" ~ amount ^^ {
    case _ ~ a => Amount.fromString(a)
  }

  def forPeople: Parser[List[String]] = "for" ~> names

  def onDesc: Parser[String] = "on" ~ """.*""".r ^^ { case _ ~ d => d.toString }

  def newLine: Parser[String] = literal("\n")

  def newLines: Parser[String] = rep1(newLine) ^^ { _ => "\n" }

  def comment: Parser[Option[Spend]] = (literal("#") ~ rep(notWhiteSpace) <~ newLines) ^^ { _ =>
    None
  }

  def spend: Parser[Option[Spend]] =
    name ~ spentAmount ~ opt(forPeople) ~ onDesc <~ newLines ^^ {
      case name ~ amount ~ Some(people) ~ desc =>
        Some(Spend(name, amount, desc, people))
      case name ~ amount ~ None ~ desc =>
        Some(Spend(name, amount, desc, Seq.empty))
    }

  def spends: Parser[List[Spend]] = rep(spend | comment) ^^ { _.flatten }

  def participants: Parser[List[String]] = "participants:" ~> names <~ newLines

  def participantsAndSpends: Parser[(List[String], List[Spend])] =
    opt(participants) ~ spends ^^ {
      case Some(participants) ~ spends =>
        participants -> spends
      case None ~ spends =>
        val participants =
          spends.flatMap { s => Seq(s.creditor) ++ s.debtors }.distinct
        participants -> spends
    }

  def parseDoc(input: Reader): (List[String], List[Spend]) = {
    parse(participantsAndSpends, input) match {
      case Success(result, _) => result
      case Failure(msg, remainder) => throw ParserFailure(msg, remainder.pos)
      case Error(msg, remainder) => throw ParserFailure(msg, remainder.pos)
    }
  }

  override val whiteSpace = """[ \t\x0B\f\r]+""".r

  val notWhiteSpace = """[^ \t\x0B\f\r\n]+""".r
}
