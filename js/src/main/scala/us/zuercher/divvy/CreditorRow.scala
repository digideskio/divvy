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

    insert(inputRow, name, isParticipant)

    return Right(CreditorRow(name, isParticipant))
  }

  def insert(inputRow: JQuery, name: String, isParticipant: Boolean) {
    val newRow = jQuery(creditorRowTemplate.format(name))
    jQuery("input:checkbox", newRow)
      .prop("checked", isParticipant)
      .change(() => DOM.updateCreditors())
    newRow.insertBefore(inputRow)
    jQuery("button.remove_creditor", newRow).click((event: Event) => DOM.removeCreditorRow(event))
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
    jQuery("div#creditor_error", inputRow).text(error)
  }

  def reset(inputRow: JQuery) {
    val inputs = jQuery("input", inputRow)
    inputs.last().prop("checked", true)
    inputs.first().value("").focus()

    setError(inputRow, "")
  }

  def unapply(input: String): Option[CreditorRow] = {
    if (!input.startsWith("C(") || !input.endsWith(")")) {
      return None
    }

    val parts = input.drop(2).dropRight(1).split(";")
    if (parts.length != 2) {
      return None
    }

    val name = EncodedString.decode(parts.head)
    val isParticipant = (parts(1) == "P")

    return Some(CreditorRow(name, isParticipant))
  }

  private val creditorRowTemplate = """<tr>
    <td>%s</td>
    <td><input type="checkbox" /></td>
    <td><button type="button" class="remove_creditor">Remove</button></td>
</tr>"""

}

case class CreditorRow(name: String, isParticipant: Boolean) {
  def serialize(): String = {
    isParticipant match {
      case true => "C(%s;P)".format(EncodedString(name))
      case false => "C(%s;X)".format(EncodedString(name))
    }
  }
}
