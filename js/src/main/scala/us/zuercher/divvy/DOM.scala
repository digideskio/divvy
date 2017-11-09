package us.zuercher.divvy

import org.scalajs.jquery.{JQuery, jQuery}
import org.scalajs.dom.{Element, Event}
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation._

object DOM {
  def main(Args: Array[String]) {
    jQuery("button.add_creditor").click(() => addNewCreditorRow())
    jQuery("button.add_expense").click(() => addNewExpenseRow())

    jQuery("form").submit((event: Event) => {
      log("form submit")
      event.preventDefault()
      false
    })

    jQuery("button.add_expense").attr("disabled", "disabled")
  }

  var logEnabled = true

  def log(s: String) {
    if (logEnabled) {
      println(s)
    }
  }

  def removeCreditorRow(event: Event) {
    event.preventDefault()

    // remove the row
    jQuery(event.target).closest("tr").detach()

    if (CreditorRow.all.filter(_.name.nonEmpty).isEmpty) {
      jQuery("button.add_expense").attr("disabled", "disabled")
    }

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
        jQuery("button.add_expense").removeAttr("disabled")
        CreditorRow.reset(inputRow)
        updateCreditors()
      }
    }
  }

  def removeExpenseRow(event: Event) {
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
    println("update creditors")

    val creds = CreditorRow.all
    val creditorNames = creds.map(_.name).sorted
    val debtorNames = creds.filter(_.isParticipant).map(_.name).sorted

    ExpenseRow.update(creditorNames, debtorNames)
  }

  def updateExpenses() {
    println("update expenses")

  }
}
