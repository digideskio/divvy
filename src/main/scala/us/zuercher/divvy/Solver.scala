package us.zuercher.divvy

import scala.annotation.tailrec

case class Matrix(
  debtors: Seq[Debtor],
  creditors: Seq[Creditor],
  txns: Map[Relationship, Payment])
{
  val nextTxnParticipants = {
    val stillDebtors = debtors.filter{ d => d.amount > Amount.zero }
    val stillCreditors = creditors.filter { c => c.amount < Amount.zero }

    val relationships =
      stillDebtors.flatMap { d =>
        stillCreditors.map { c =>
	  d -> c
        }
      }

    relationships.find { case (debtor, creditor) =>
      !txns.contains(Relationship(debtor.name, creditor.name))
    }
  }

  // None implies the next transaction is not possible
  val maxPayment: Option[Amount] = {
    nextTxnParticipants.map { case (debtor, creditor) =>
      debtor.amount.abs min creditor.amount.abs
    }
  }

  def isSolved: Boolean = {
    val totalDebt = debtors.map { _.amount }.reduceLeft { _ + _ }
    val totalCredit = creditors.map { _.amount }.reduceLeft { _ + _ }

    totalDebt == Amount.zero && totalCredit == Amount.zero
  }

  def makePayment(amount: Amount): Option[Matrix] = {
    if (nextTxnParticipants.isEmpty)
      return None

    val (debtor, creditor) = nextTxnParticipants.get

    if (debtor.amount < amount) return None
    if (creditor.amount.abs < amount) return None

    val newDebtors =
      debtors.map { d =>
        if (d == debtor) {
	  d.pays(amount)
	} else {
          d
	}
      }

    val newCreditors =
      creditors.map { c =>
        if (c == creditor) {
          c.isPaid(amount)
        } else {
          c
        }
      }

    val parties = Relationship(debtor.name, creditor.name)
    val newTxns = txns + (parties -> Payment(parties, amount))

    Some(Matrix(newDebtors, newCreditors, newTxns))
  }
}

object Solver {
  def apply(debtors: Seq[Debtor], creditors: Seq[Creditor]): Seq[Payment] = {
    val best =
      debtors.permutations.foldLeft(None: Option[Matrix]) { (best, d) =>
        creditors.permutations.foldLeft(best) { (best, c) =>
          val m = Matrix(d, c, Map.empty)
          val solution = solve(m)
          (best, solution) match {
            case (None, s) => s
            case (b, None) => b
            case (Some(b), Some(s)) => Some(pickBest(b, s))
          }
        }
      }

    best.map { _.txns.values.toSeq }.getOrElse(Seq.empty)
  }

  def pickBest(a: Matrix, b: Matrix): Matrix = {
    if (a.txns.size < b.txns.size) {
      a
    } else if (a.txns.size > b.txns.size) {
      b
    } else {
      if (spread(a.txns.values.toSeq) <= spread(b.txns.values.toSeq)) a else b
    }
  }

  def spread(s: Seq[Payment]): Amount = {
    val amounts = s.map { _.amount }.filter { _ != Amount.zero }
    val min = amounts.reduceLeft { _ min _ }
    val max = amounts.reduceLeft { _ max _ }
    max - min
  }

  @tailrec
  def solve(matrix: Matrix): Option[Matrix] = {
    val maxPayment = matrix.maxPayment
    val nextMatrix =
      maxPayment.flatMap { payment =>
        matrix.makePayment(payment)
      }
    if (nextMatrix.forall { _.isSolved }) {
      nextMatrix
    } else {
      solve(nextMatrix.get)
    }
  }
}
