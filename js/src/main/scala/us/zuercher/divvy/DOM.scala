package us.zuercher.divvy

import org.scalajs.jquery.{JQuery, jQuery}
import org.scalajs.dom
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation._

object DOM {
  def main(Args: Array[String]) {
    jQuery("button.add_creditor").click(() => addNewCreditorRow())
    jQuery("button.add_expense").click(() => addNewExpenseRow())
    jQuery("button.restart").click(() => {
      dom.window.location.href = "%s%s".format(
        dom.window.location.origin,
        dom.window.location.pathname
      )
    })
    jQuery("form").submit((event: dom.Event) => {
      event.preventDefault()
      false
    })
    jQuery("""form#ex select[name="ex.debtors"]""").change(ExpenseRow.handleDebtorSelect _)

    jQuery("button.add_expense").attr("disabled", "disabled")

    // Load data from the URL.
    val href = dom.window.location.href.toString()
    val query = href.dropWhile(_ != '?').drop(1)
    val params = query.split('&').map(param => {
      val (k, v) = param.span(_ != '=')
      (k, v.drop(1))
    }).toMap

    params.get("data").foreach(v => {
      val (creditors, expenses) = Storage.deserialize(v)

      val tbody = jQuery("table#creditors tbody")
      creditors.foreach(c => {
        CreditorRow.insert(tbody.children("tr").last(), c.name, c.isParticipant)
        updateCreditors()
      })

      val allCreds = CreditorRow.all
      val tbody2 = jQuery("table#expenses tbody")
      expenses.foreach(e => {
        ExpenseRow.insert(
          tbody2.children("tr").last(), allCreds, e.creditor, e.amount, e.description, e.debtors)
        updateExpenses()
      })
    })
  }

  var logEnabled = false

  def log(s: String) {
    if (logEnabled) {
      println(s)
    }
  }

  def removeCreditorRow(event: dom.Event) {
    event.preventDefault()

    // remove the row
    jQuery(event.target).closest("tr").detach()

    updateCreditors()
  }

  private def addNewCreditorRow() {
    val tbody = jQuery("table#creditors tbody")
    val rows = tbody.children("tr")
    val inputRow = rows.last()

    CreditorRow(inputRow) match {
      case Left(error) =>
        CreditorRow.setError(inputRow, error)

      case Right(_) => {
        CreditorRow.reset(inputRow)
        updateCreditors()
      }
    }
  }

  def removeExpenseRow(event: dom.Event) {
    event.preventDefault()
    jQuery(event.target).closest("tr").detach();

    updateExpenses()
  }

  private def addNewExpenseRow() {
    val tbody = jQuery("table#expenses tbody")
    val rows = tbody.children("tr")
    val inputRow = rows.last()

    ExpenseRow(inputRow, CreditorRow.all) match {
      case Left(error) =>
        ExpenseRow.setError(inputRow, error)

      case Right(_) => {
        ExpenseRow.reset(inputRow)
        updateExpenses()
      }
    }
  }

  def updateCreditors() {
    log("update creditors")

    val creds = CreditorRow.all

    val creditorNames = creds.map(_.name).sorted
    val debtorNames = creds.filter(_.isParticipant).map(_.name).sorted

    ExpenseRow.update(creditorNames, debtorNames)

    if (creds.filter(_.name.nonEmpty).isEmpty) {
      jQuery("button.add_expense").attr("disabled", "disabled")
    } else {
      jQuery("button.add_expense").removeAttr("disabled")
    }

    updateExpenses()
  }

  private def spendComparator(a: Spend, b: Spend): Boolean = {
    a.creditor.compare(b.creditor) match {
      case i if i < 0 => return true
      case i if i > 0 => return false
      case 0 => ()
    }

    if (a.amount > b.amount) {
      return false
    } else if (a.amount < b.amount) {
      return true
    }

    a.desc.compare(b.desc) match {
      case i if i < 0 => return true
      case i if i > 0 => return false
      case 0 => ()
    }

    val ad = a.debtors.sorted.mkString("|")
    val bd = b.debtors.sorted.mkString("|")
    ad.compare(bd) match {
      case i if i < 0 => return true
      case i if i >= 0 => return false
    }
  }

  def updateExpenses() {
    log("update expenses")

    val participants = CreditorRow.all.filter(_.isParticipant).map(_.name).sorted

    val spend = ExpenseRow.all.map(ex => {
      if (ex.debtors.contains("@ALL@")) {
        Spend(ex.creditor, ex.amount, ex.description, participants)
      } else {
        Spend(ex.creditor, ex.amount, ex.description, ex.debtors)
      }
    }).sortWith(spendComparator)

    if (spend.nonEmpty) {
      val payments = Divvy(participants, spend, false)

      val paymentsByDebtor = payments.groupBy(_.parties.debtor)

      val results = jQuery("table#results tbody")
      results.empty()

      paymentsByDebtor.keys.toSeq.sorted.map(paymentsByDebtor(_)).zipWithIndex.foreach({
        case (paymentSeq, i) => {
          val cls = if (i % 2 == 0) "even" else "odd"
          if (paymentSeq.nonEmpty) {
            results.append(jQuery(
              """<tr class="%s"><td rowspan="%d">%s</td><td>%s</td><td>$%s</td></tr>""".format(
                cls,
                paymentSeq.length,
                paymentSeq.head.parties.debtor,
                paymentSeq.head.parties.creditor,
                paymentSeq.head.amount.toString()
              )
            ))

            paymentSeq.tail.foreach(payment => {
              results.append(jQuery(
                """<tr class="%s"><td>%s</td><td>$%s</td></tr>""".format(
                  cls,
                  payment.parties.creditor,
                  payment.amount.toString()
                )
              ))
            })
          }
        }
      })
    }

    if (participants.nonEmpty) {
      updateLink()
    }
  }

  def updateLink() {
    val serialized = Storage.serialize(CreditorRow.all, ExpenseRow.all)

    val url = "%s%s?data=%s".format(
      dom.window.location.origin,
      dom.window.location.pathname,
      serialized)

    val anchor = jQuery("""div#link a""")
    anchor.attr("href", url)
    jQuery("div.permalink", anchor).text(url)
  }
}
