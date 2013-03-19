package csv

sealed trait Value[A] {
  def serialize(a: A): String
  def deserialize(value: String): Option[A]
}

object Value {
  final def apply[A](implicit A: Value[A]) = A

  final def serialize[A: Value](a: A): String =
    Value[A].serialize(a)

  final def deserialize[A: Value](v: String): Option[A] =
    Value[A].deserialize(v)

  implicit object IntValue extends Value[Int] {
    private val IsInt = """(-?\d+)""".r
    def serialize(a: Int): String = a.toString
    def deserialize(value: String): Option[Int] = {
      value match {
        case IsInt(n) => Some(n.toInt)
        case _ => None
      }
    }
  }

  implicit object DoubleValue extends Value[Double] {
    def serialize(x: Double) = x.toString
    def deserialize(value: String) = try {
      Some(value.toDouble)
    } catch { case nfe: NumberFormatException =>
      None
    }
  }

  implicit object StringValue extends Value[String] {
    def serialize(a: String): String = a
    def deserialize(value: String): Option[String] = Some(value)
  }

  implicit def OptValue[A: Value] = new Value[Option[A]] {
    def serialize(a: Option[A]) = a map Value[A].serialize getOrElse ""
    def deserialize(value: String): Option[Option[A]] = value match {
      case "" => Some(None)
      case v => Value.deserialize[A](v) map (Some(_): Option[A])
    }
  }
}
