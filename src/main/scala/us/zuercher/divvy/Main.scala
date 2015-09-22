package us.zuercher.divvy

import com.twitter.app.{App, Flag}
import java.io.{File, InputStreamReader}

object Main extends App {
  val verbose = flag("v", false, "enables verbose mode")

  def main() {
    if (args.isEmpty) {
      (divvy _).tupled(Spend.parse(new InputStreamReader(System.in)))
    } else {
      args foreach { inputFile =>
        try {
          (divvy _).tupled(Spend.parse(new File(inputFile)))
        }
        catch { case e: Exception =>
          println("%s failed: %s".format(inputFile, e.getMessage))
          e.printStackTrace()
        }
      }
    }
  }

  def divvy(participants: Seq[String], spend: Seq[Spend]) {
    val payments = Divvy(participants, spend, verbose())
    payments.foreach { p =>
      println(
        s"${p.parties.debtor} pays ${p.parties.creditor} ${p.amount}"
      )
    }
  }
}
