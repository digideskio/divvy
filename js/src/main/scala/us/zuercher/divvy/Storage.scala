package us.zuercher.divvy

object EncodedString {
  def decode(s: String): String = {
    val parts = s.split("%")
    val d = parts.drop(1).map(p => {
      val code = p.take(4)
      val rest = p.drop(4)

      try {
        "%c%s".format(Integer.parseInt(code, 16).toChar, rest)
      } catch {
        case _: Throwable => rest
      }
    })

    (parts(0) ++ d).mkString("")
  }
}

case class EncodedString(s: String) {
  override def toString(): String = {
    s.flatMap({
      case ch @ ('%' | ',' | '(' | ')' | '[' | ']' | ';' | '\n') =>
        "%%%04X".format(ch.toInt & 0xFFFF)
      case ch => "%c".format(ch)
    })
  }
}

object Base64 {
  private val encodeTbl = Array[Char](
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
    'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
    'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
    'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/' )

  private val decodeTbl = Array[Int](
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, 52, 53, 54,
    55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2,
    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, 26, 27, 28, 29, 30,
    31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)

  private val encodeShifts = Seq(18, 12, 6, 0)
  private val decodeShifts = Seq(16, 8, 0)

  def encode(data: Array[Byte]): String = {
    val buffer = new StringBuilder(data.length * 4 / 3 + 1)
    data.toSeq.grouped(3).foreach(g => {
      val (b, n) =
        g match {
          case Seq(x, y, z) =>
            (((x.toInt & 0xFF) << 16) | ((y.toInt & 0xFF) << 8) | (z.toInt & 0xFF), 4)
          case Seq(x, y) =>
            (((x.toInt & 0xFF) << 16) | ((y.toInt & 0xFF) << 8), 3)
          case Seq(x) =>
            ((x.toInt & 0xFF) << 16, 2)
        }

      encodeShifts.take(n).foreach(shft => {
        val c = encodeTbl((b >> shft) & 0x3F)
        buffer.append(c)
      })
      encodeShifts.drop(n).foreach(_ => { buffer.append('=') })
    })

    buffer.toString()
  }

  def decode(data: String): Array[Byte] = {
    val stripped = data.getBytes("UTF-8").filter(b => { decodeTbl(b.toInt & 0xFF) != -1 })

    stripped.toSeq.grouped(4).flatMap(g => {
      val (b, n) = g match {
        case Seq(w, x, y, z) =>
          (
            ((decodeTbl(w.toInt) & 0x3F) << 18) |
              ((decodeTbl(x.toInt) & 0x3F) << 12) |
              ((decodeTbl(y.toInt) & 0x3F) << 6) |
              (decodeTbl(z.toInt) & 0x3F),
            3
          )

        case Seq(w, x, y) =>
          (
            ((decodeTbl(w.toInt) & 0x3F) << 18) |
              ((decodeTbl(x.toInt) & 0x3F) << 12) |
              ((decodeTbl(y.toInt) & 0x3F) << 6),
            2
          )

        case Seq(w, x) =>
          (
            ((decodeTbl(w.toInt) & 0x3F) << 18) |
              ((decodeTbl(x.toInt) & 0x3F) << 12),
            1
          )

        case Seq(w) =>
          (
            (decodeTbl(w.toInt) & 0x3F) << 18,
            0
          )
      }

      decodeShifts.take(n).map { shft =>
        ((b >> shft) & 0xFF).toByte
      }
    }).toArray
  }
}

object Storage {
  def serialize(creditors: Seq[CreditorRow], expenses: Seq[ExpenseRow]): String = {
    val serialized =
      "%s\n%s\n".format(
        creditors.map(_.serialize).mkString("\n"),
        expenses.map(_.serialize).mkString("\n")
      )

    return Base64.encode(serialized.getBytes("UTF-8"))
  }

  def deserialize(s: String): (Seq[CreditorRow], Seq[ExpenseRow]) = {
    val formatted = new String(Base64.decode(s), "UTF-8")

    val lines = formatted.split("\n")
    val (formattedCreditors, formattedExpenses) = lines.partition(CreditorRow.unapply(_).nonEmpty)

    val creditors = formattedCreditors.flatMap(CreditorRow.unapply(_))
    val expenses = formattedExpenses.flatMap(ExpenseRow.unapply(_))

    (creditors, expenses)
  }
}
