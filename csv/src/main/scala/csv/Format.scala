package csv

final case class Format(sep: Char) {
  def toRow[A: SepValues](a: A): String =
    SepValues.serialize(this)(a).getOrElse("")

  def fromRow[A: SepValues](row: String): Option[A] =
    SepValues.deserialize[A](this)(row)

  private val Uncons = s"""("(?:[^"]|"")*"|[^$sep]*)(?:$sep?)(.*)""".r
  private val Quoted = """"(.*)"""".r

  private def quote(value: String): String = {
    if (value.contains(sep) || value.contains('"')) {
      "\"%s\"" format value.replace("\"", "\"\"")
    } else {
      value
    }
  }

  private def unquote(value: String): String = value match {
    case Quoted(inner) => inner.replace("\"\"", "\"")
    case value => value
  }

  def cons(head: String, tailM: Option[String]): String =
    tailM map { tail => s"${quote(head)}$sep$tail" } getOrElse quote(head)

  def uncons(row: String): Option[(String, String)] = row match {
    case "" => None
    case Uncons(head, tail) => Some(unquote(head) -> tail)
    case _ => sys.error("Blagawaga!")
  }

  def append(row0: String, row1: String): String =
    row0 + sep + row1
}

object Format {
  val CSV = Format(',')
  val TSV = Format('\t')
}
