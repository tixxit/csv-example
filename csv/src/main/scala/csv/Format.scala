package csv

final case class Format(sep: Char) {
  def toRow[A: SepValues](a: A): String =
    SepValues.serialize(this)(a).getOrElse("")

  def fromRow[A: SepValues](row: String): Option[A] =
    SepValues.deserialize[A](this)(row)

  // private val Uncons = s"""([^${sep}]*)(?:${sep}?)(.*)""".r
  private val Uncons = ("""("(?:[^"]|"")*"|[^%c]*)(?:%c?)(.*)""" format (sep, sep)).r
  private val Quoted = """"(.*)"""".r

  private def normalize(value: String): String = {
    if (value.indexOf(sep) >= 0 || value.indexOf('"') >= 0) {
      "\"%s\"" format value.replace("\"", "\"\"")
    } else {
      value
    }
  }

  def cons(head: String, tail: String): String =
    "%s%c%s" format (normalize(head), sep, tail)

  def uncons(row: String): Option[(String, String)] = row match {
    case "" => None
    case Uncons(Quoted(head), tail) => Some(head -> tail)
    case Uncons(head, tail) => Some(head -> tail)
    case _ => sys.error("Blagawaga!")
  }

  def append(row0: String, row1: String): String =
    row0 + sep + row1
}

object Format {
  val CSV = Format(',')
  val TSV = Format('\t')
}
