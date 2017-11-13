package us.zuercher.divvy

import org.scalajs.jquery.{JQuery, jQuery}
import org.scalajs.dom
import scala.scalajs.js

object ExpenseRow {
  def apply(inputRow: JQuery, creditors: Seq[CreditorRow]): Either[String, ExpenseRow] = {
    val creditor =
      Option(jQuery("""select[name="ex.creditor"]""", inputRow).value()).map(_.toString())
    if (creditor.isEmpty || creditor.get.isEmpty) {
      return Left("No creditor selected.")
    }

    val amount = Option(jQuery("""input[name="ex.amount"]""", inputRow).value()).map(_.toString())
    if (amount.isEmpty || amount.get.isEmpty) {
      return Left("No amount given.")
    }

    val amountValue = amount.flatMap(Amount.maybeFromString(_))
    if (amountValue.isEmpty) {
      return Left("Invalid amount given.")
    }

    val desc =
      Option(jQuery("""input[name="ex.description"]""", inputRow).value()).map(_.toString()).getOrElse("")

    val debtors =
      Option(jQuery("""select[name="ex.debtors"]""", inputRow).value())
        .map(_.asInstanceOf[js.Array[String]])
        .getOrElse(js.Array[String]())
    if (debtors.isEmpty) {
      return Left("No debtor(s) selected.")
    }
    insert(inputRow, creditors, creditor.get, amountValue.get, desc, debtors.toSeq)

    Right(ExpenseRow(creditor.get, amountValue.get, desc, debtors.toSeq))
  }

  def insert(
    inputRow: JQuery,
    creditors: Seq[CreditorRow],
    creditor: String,
    amount: Amount,
    desc: String,
    debtors: Seq[String])
  {
    val newRow = jQuery(expenseRowTemplate.format(amount, desc))
    val credInput = jQuery("""select[name="ex.creditor"]""", newRow)
    credInput.empty()
    credInput.append(
      js.Array(creditors.map { c => jQuery(expenseMemberTemplate.format(c.name)) }: _*))
    credInput.value(creditor)
    credInput.change(() => DOM.updateExpenses())

    val amountInput = jQuery("""input[name="ex.amount"]""", newRow)
    amountInput.change(() => DOM.updateExpenses())

    val descInput = jQuery("""input[name="ex.description"]""", newRow)
    descInput.change(() => DOM.updateExpenses())

    val debtInput = jQuery("""select[name="ex.debtors"]""", newRow)
    debtInput.empty()
    debtInput.append(
      js.Array(
        Seq(jQuery(allDebtors)) ++
          creditors
            .filter { _.isParticipant }
            .map { c => jQuery(expenseMemberTemplate.format(c.name)) }: _*))
    debtInput.value(js.Array(debtors: _*)).change((event: dom.Event) => {
      handleDebtorSelect(event)
      DOM.updateExpenses()
    })

    newRow.insertBefore(inputRow)
    jQuery("button.remove_expense", newRow).click((event: dom.Event) => DOM.removeExpenseRow(event))
  }

  // Make sure the "all participants" value is mutually exclusive to other debtors.
  def handleDebtorSelect(event: dom.Event) {
    val target = jQuery(event.target)
    val values = target.value().asInstanceOf[js.Array[String]]
    if (values.length > 1 && values.contains(allDebtorsValue)) {
      target.value(values.filter(_ != allDebtorsValue))
    }
  }

  def all: Seq[ExpenseRow] = {
    val rows = jQuery("""table#expenses tbody tr[class!="input"]""")
    var c = Seq[ExpenseRow]()
    rows.each((row: dom.Element) => {
      val creditor = jQuery("""td select[name="ex.creditor"]""", row).value().toString()
      val amount =
        Amount.maybeFromString(
          jQuery("""td input[name="ex.amount"]""", row).value().toString()
        ).getOrElse(Amount.zero)
      val desc = jQuery("""td input[name="ex.description"]""", row).value().toString()
      val debtors =
        jQuery("""td select[name="ex.debtors"]""", row).value().asInstanceOf[js.Array[String]]

      c = c ++ Seq(ExpenseRow(creditor, amount, desc, debtors.toSeq))
    })
    c
  }

  def update(creditorNames: Seq[String], debtorNames: Seq[String]) {
    jQuery("table#expenses tbody tr").each((row: dom.Element) => {
      val credInput = jQuery("""select[name="ex.creditor"]""", row)
      val currentCreditor = Option(credInput.value()).map(_.toString).getOrElse("")

      val creditorOptions = creditorNames.map(s => jQuery(expenseMemberTemplate.format(s)))
      credInput.empty()
      credInput.append(js.Array(creditorOptions: _*))

      if (creditorNames.contains(currentCreditor)) {
        credInput.value(currentCreditor)
      } else {
        credInput.value(null: String)
      }

      val debtorOptions =
        Seq(jQuery(allDebtors)) ++ debtorNames.map(s => jQuery(expenseMemberTemplate.format(s)))

      val debtorInput = jQuery("""select[name="ex.debtors"]""", row)
      val currentDebtors =
        Option(debtorInput.value())
          .map(_.asInstanceOf[js.Array[String]])
          .getOrElse(js.Array[String]())
          .filter(debtor => { debtor == allDebtorsValue || debtorNames.contains(debtor) })

      debtorInput.empty()
      debtorInput.append(js.Array(debtorOptions: _*))
      if (currentDebtors.nonEmpty) {
        debtorInput.value(currentDebtors)
      } else {
        debtorInput.value(null: js.Array[String])
      }
    })
  }

  def setError(inputRow: JQuery, error: String) {
    jQuery("div#expense_error", inputRow).text(error)
  }

  def reset(inputRow: JQuery) {
    jQuery("""td select[name="ex.creditor"]""", inputRow).value(null: String)
    jQuery("""td input[name="ex.amount"]""", inputRow).value("")
    jQuery("""td input[name="ex.description"]""", inputRow).value("")
    jQuery("""td select[name="ex.debtors"]""", inputRow).value(js.Array(allDebtorsValue))

    setError(inputRow, "")
  }

  def unapply(input: String): Option[ExpenseRow] = {
    if (!input.startsWith("E(") || !input.endsWith(")")) {
      return None
    }

    val parts = input.drop(2).dropRight(1).split(";")
    if (parts.length != 4) {
      return None
    }

    val name = EncodedString.decode(parts.head)
    val amount = EncodedString.decode(parts(1))
    val desc = EncodedString.decode(parts(2))

    if (!parts(3).startsWith("[") || !parts(3).endsWith("]")) {
      return None
    }

    val debtors = parts(3).drop(1).dropRight(1).split(",").map(EncodedString.decode(_))
    if (debtors.isEmpty) {
      return None
    }

    val amountVal = Amount.maybeFromString(amount)
    if (amountVal.isEmpty) {
      return None
    }

    return Some(ExpenseRow(name, amountVal.get, desc, debtors))
  }

  private val expenseRowTemplate = """<tr>
    <td><select title="The expense's creditor." name="ex.creditor"></select></td>
    <td>$<input type="text" title="The expense amount." name="ex.amount" value="%s"/></td>
    <td><input type="text" title="A description of the expense." name="ex.description" value="%s" /></td>
    <td><select title="Debtors responsible for this expense." name="ex.debtors" title="Who owes for this item?" multiple="true">""" + allDebtors + """</select></td>
    <td><button type="button" class="remove_expense">Remove</button></td>
</tr>
"""

  private val allDebtorsValue = "@ALL@"
  private val allDebtors =
    """<option value="""" + allDebtorsValue + """">All Partcipants</option>"""
  private val expenseMemberTemplate = """<option value="%1$s">%1$s</option>"""
}

case class ExpenseRow(
  creditor: String,
  amount: Amount,
  description: String,
  debtors: Seq[String])
{
  def serialize(): String = {
    """E(%s;%s;%s;[%s])""".format(
      EncodedString(creditor),
      EncodedString(amount.toString()),
      EncodedString(description),
      debtors.map(EncodedString(_).toString()).mkString(",")
    )
  }
}
