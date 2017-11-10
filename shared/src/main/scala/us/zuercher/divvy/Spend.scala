package us.zuercher.divvy

import java.io.{File, FileReader, Reader}

case class Spend(
  creditor: String,
  amount: Amount,
  desc: String,
  debtors: Seq[String])

object Spend {
  def parse(file: String): (Seq[String], Seq[Spend]) = parse(file)

  def parse(file: File): (Seq[String], Seq[Spend]) = parse(new FileReader(file))

  def parse(input: Reader): (Seq[String], Seq[Spend]) = {
    try {
      new Parser().parseDoc(input)
    }
    catch {
      case e: ParserFailure =>
        println(
          "Parser Error:\n  %s\n  at: line %d, column %d".format(
            e.getMessage,
            e.pos.line,
            e.pos.column
          )
        )
        throw e
    }
  }
}
