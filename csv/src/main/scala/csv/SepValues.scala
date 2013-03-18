package csv

import scalaz.syntax.monad._
import scalaz.std.option._
import shapeless._

sealed trait SepValues[A] {
  def serialize(format: Format)(a: A): Option[String]
  def deserialize(format: Format)(values: String): Option[A]
}

object SepValues extends SepValues1 {
  final def apply[A](implicit A: SepValues[A]) = A

  final def serialize[A: SepValues](format: Format)(a: A): Option[String] =
    SepValues[A].serialize(format)(a)
  final def deserialize[A: SepValues](format: Format)(v: String): Option[A] =
    SepValues[A].deserialize(format)(v)

  implicit def hnil: SepValues[HNil] = new SepValues[HNil] {
    private val Whitespace = """(\s*)""".r

    def serialize(format: Format)(nil: HNil): Option[String] = None
    def deserialize(format: Format)(values: String) = values match {
      case Whitespace(_) => Some(HNil)
      case _ => None
    }
  }

  implicit def hlist[H: Value, T <: HList: SepValues]: SepValues[H :: T] =
    new SepValues[H :: T] {
      def serialize(format: Format)(h: H :: T): Option[String] = {
        val head = Value.serialize(h.head)
        val tail = SepValues.serialize(format)(h.tail)
        tail map (format.cons(head, _)) orElse Some(head)
      }

      def deserialize(format: Format)(values: String): Option[H :: T] = {
        format.uncons(values) flatMap { case (head, tail) =>
          val headOpt = Value.deserialize[H](head)
          val tailOpt = SepValues.deserialize[T](format)(tail)
          (headOpt |@| tailOpt)(_ :: _)
        }
      }
    }
}

trait SepValues1 extends SepValues0 {
  implicit def appender[A, S <: HList, // What we know up-front.
                        P <: HList,
                        L <: HList,
                        N <: Nat](implicit
      iso: Iso[A, P],
      prepend: PrependAux[P, S, L],
      lenOfP: LengthAux[P, N],
      take: TakeAux[L, N, P],
      drop: DropAux[L, N, S],
      sv: SepValues[L]): SepValues[A :: S] = {

    new SepValues[A :: S] {
      def serialize(format: Format)(h: A :: S): Option[String] =
        sv.serialize(format)(prepend(iso.to(h.head), h.tail))

      def deserialize(format: Format)(values: String): Option[A :: S] =
        sv.deserialize(format)(values) map { hs =>
          iso.from(take(hs)) :: drop(hs)
        }
    }
  }
}

trait SepValues0 {
  implicit def iso[A, L <: HList](implicit iso: Iso[A, L], sv: SepValues[L]): SepValues[A] =
    new SepValues[A] {
      def serialize(format: Format)(a: A): Option[String] =
        sv.serialize(format)(iso.to(a))
      def deserialize(format: Format)(values: String): Option[A] =
        sv.deserialize(format)(values) map iso.from
    }
}
