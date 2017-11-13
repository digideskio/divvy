package us.zuercher.divvy

import java.io.{File, FileReader, Reader}

case class Spend(
  creditor: String,
  amount: Amount,
  desc: String,
  debtors: Seq[String])

object Spend {
  def parse(file: String): Seq[Spend] = parse(file)

  def parse(file: File): Seq[Spend] = parse(new FileReader(file))

  def parse(input: Reader): Seq[Spend] = {
    try {
      val (participants, spend) = new Parser().parseDoc(input)

      spend.map {
        case Spend(cred, amt, desc, debtors) if debtors.isEmpty =>
          Spend(cred, amt, desc, participants)
      }
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
