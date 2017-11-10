package us.zuercher.divvy

case class Creditor(name: String, amount: Amount) {
  def isPaid(payment: Amount) = Creditor(name, amount + payment)
}

case class Debtor(name: String, amount: Amount) {
  def pays(payment: Amount) = Debtor(name, amount - payment)
}

case class Relationship(debtor: String, creditor: String)

case class Payment(parties: Relationship, amount: Amount)

object Divvy {
  def apply(participants: Seq[String], spend: Seq[Spend], verbose: Boolean): Seq[Payment] = {
    val d = new Divvy(participants, spend, verbose)
    d()
  }
}

class Divvy(participants: Seq[String], rawSpend: Seq[Spend], verbose: Boolean = false) {
  Amount.reset()

  val spend = rawSpend.filter {
    case Spend(c, _, _, d) if d.size == 1 => c != d.head
    case _ => true
  }

  require(
    spend.flatMap { _.debtors }.distinct.diff(participants).isEmpty,
    "debtors must be participants"
  )

  val nonParticipants = spend.map { _.creditor }.distinct.diff(participants)

  val totalSpent = spend.map { _.amount }.reduceLeft { _ + _ }

  val spentPerPerson =
    spend.foldLeft(Map.empty[String, Amount]) { case (m, spend) =>
      val current = m.getOrElse(spend.creditor, Amount.zero)
      m + (spend.creditor -> (current + spend.amount))
    }

  val participantShares =
    spend.foldLeft(Map.empty[String, Amount]) { case (m, spend) =>
      val debtors = if (spend.debtors.isEmpty) participants else spend.debtors

      val splits = spend.amount.split(debtors.length)

      debtors.zip(splits).foldLeft(m) { case (m, (name, split)) =>
        val current = m.getOrElse(name, Amount.zero)
        val updated = current + split
        m + (name -> updated)
      }
    }

  val nonParticipantShares = nonParticipants.map { _ -> Amount.zero }.toMap

  val shares = participantShares ++ nonParticipantShares

  val (debtors, creditors) = {
    val nets =
      shares.map { case (name, share) =>
        val spent = spentPerPerson.getOrElse(name, Amount.zero)
        name -> (share - spent)
      }

    val (d, c) =
      nets.filter {
        case (_, net) => net != Amount.zero
      }.partition {
        case (_, net) => net > Amount.zero
      }

    (
      d.map { case (name, net) => Debtor(name, net) }.toSeq,
      c.map { case (name, net) => Creditor(name, net) }.toSeq
    )
  }

  val totalDebt = debtors.map { _.amount }.reduceLeft { _ + _ }

  val totalCredit = creditors.map { _.amount}.reduceLeft { _ + _ }

  def apply(): Seq[Payment] = {
    echo(s"Total Spent: $totalSpent")

    echo(s"Participants: ${participants.sorted.mkString(", ")}")

    echo("Spent:")
    spentPerPerson.foreach { case (name, amt) =>
      echo(s"  $name spent $amt")
    }

    echo("Shares:")
    shares.foreach { case (name, amount) =>
      echo(s"  $name's share is $amount")
    }

    echo("Debtors:")
    debtors.foreach {
      case Debtor(name, amount) => echo(s"  $name owes $amount")
    }

    echo("Creditors:")
    creditors.foreach {
      case Creditor(name, amount) => echo(s"  $name is owed ${-amount}")
    }

    echo(s"Total Debt: $totalDebt")
    echo(s"Total Credit: ${-totalCredit}")

    Solver(debtors, creditors)
  }

  def echo(s: String) {
    if (verbose) println(s)
  }
}
