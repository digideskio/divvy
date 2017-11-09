package us.zuercher.divvy

import org.scalajs.jquery.{JQuery, jQuery}
import org.scalajs.dom.{Element, Event}
import scala.scalajs.js

object CreditorRow {
  def apply(inputRow: JQuery): Either[String, CreditorRow] = {
    val inputs = jQuery("input", inputRow)
    val name = inputs.first().value().toString();
    val isParticipant = inputs.last().prop("checked").asInstanceOf[Boolean]

    if (name.isEmpty) {
      return Left("Creditor name cannot be empty")
    }

    val newRow = jQuery(creditorRowTemplate.format(name))
    jQuery("input:checkbox", newRow)
      .prop("checked", isParticipant)
      .change(() => DOM.updateCreditors())
    newRow.insertBefore(inputRow)
    jQuery("button.remove_creditor", newRow).click((event: Event) => DOM.removeCreditorRow(event))

    return Right(CreditorRow(name, isParticipant))
  }

  def all: Seq[CreditorRow] = {
    val rows = jQuery("""table#creditors tbody tr[class!="input"]""")
    var c = Seq[CreditorRow]()
    rows.each((row: Element) => {
      val cols = jQuery("td", row)
      val name = cols.first().text().toString()
      val isParticipant = jQuery("input", cols).first().prop("checked")
      c = c ++ Seq(CreditorRow(name, isParticipant.asInstanceOf[Boolean]))
    })
    c
  }

  def setError(inputRow: JQuery, error: String) {
    jQuery("td:last", inputRow).text(error)
  }

  def reset(inputRow: JQuery) {
    val inputs = jQuery("input", inputRow)
    inputs.last().prop("checked", true)
    inputs.first().value("").focus()

    setError(inputRow, "")
  }

  private val creditorRowTemplate = """<tr>
    <td>%s</td>
    <td><input type="checkbox" /></td>
    <td><button type="button" class="remove_creditor">Remove</button></td>
    <td></td>
</tr>"""

}

case class CreditorRow(name: String, isParticipant: Boolean)
