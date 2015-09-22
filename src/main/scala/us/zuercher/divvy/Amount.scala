package us.zuercher.divvy

import scala.util.Random

case class Amount(inCents: Int) {
  def this(d: Int, c: Int) =
    this((math.abs(d) * 100 + math.abs(c)) * (math.signum(d) | 1))

  def +(other: Amount) = new Amount(this.inCents + other.inCents)

  def -(other: Amount) = new Amount(this.inCents - other.inCents)

  def unary_- = new Amount(-this.inCents)

  def abs = new Amount(math.abs(this.inCents))

  def max(other: Amount) = if (this > other) this else other
  def min(other: Amount) = if (this < other) this else other

  def /(n: Int) = {
    val result = math.round(inCents.toFloat / n.toFloat)
    Amount(result)
  }

  def *(n: Int) = Amount(this.inCents * n)

  /**
   * Divide this amount into N parts, insuring that the sum of the parts is
   * guaranteed to equal the original amount. Splitting 10.00 in 2 results in
   * 5.00 and 5.00. Splitting 10.00 in 3 results in 3.33, 3.33, and 3.34.
   */
  def split(n: Int): Seq[Amount] = {
    require(n > 0)

    val base = this / n
    val sum = base * n
    if (sum == this) {
      return Seq.fill(n)(base)
    }

    val (numToAdjust, adjustment) =
      if (sum > this) {
        ((sum - this).inCents, -Amount.cent)
      } else {
	((this - sum).inCents, Amount.cent)
      }

    val result =
      Seq.fill(numToAdjust)(base + adjustment) ++
      Seq.fill(n - numToAdjust)(base)

    Random.shuffle(result)
  }

  def >(other: Amount) = this.inCents > other.inCents
  def >=(other: Amount) = this.inCents >= other.inCents
  def <(other: Amount) = this.inCents < other.inCents
  def <=(other: Amount) = this.inCents <= other.inCents

  override def toString = {
    val dollars = math.abs(inCents) / 100
    val cents = math.abs(inCents) % 100
    if (inCents < 0) {
      "-%d.%02d".format(dollars, cents)
    } else {
      "%d.%02d".format(dollars, cents)
    }
  }
}

object Amount {
  val DollarsAndCents = """^\$?([0-9]+)\.([0-9]{2})$""".r
  val Dollars = """^\$?([0-9]+)$""".r
  val Cents = """^\$?\.([0-9]{2})$""".r

  val zero = Amount(0)

  val cent = Amount(1)

  def fromString(str: String): Amount = {
    str match {
      case DollarsAndCents(d, c) => new Amount(d.toInt, c.toInt)
      case Dollars(d)            => new Amount(d.toInt, 0)
      case Cents(c)              => Amount(c.toInt)
    }
  }

  def lt(a: Amount, b: Amount) = a < b
}
