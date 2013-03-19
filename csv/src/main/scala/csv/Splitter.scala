package csv

import shapeless._

trait Splitter[P <: HList, S <: HList, L <: HList] {
  def conjoin(p: P, s: S): L
  def disjoin(l: L): (P, S)
}

object Splitter {
  //implicit def s0[L <: HList] = new Splitter[HNil, L, L] {
  //  def conjoin(p: HNil, s: L) = s
  //  def disjoin(l: L) = (HNil, l)
  //}

  //implicit def s1[H, T <: HList, L <: HList, O <: HList](implicit
  //    splitter0: Splitter[T, L, O]) = {
  //  new Splitter[H :: T, L, H :: O] {
  //    def conjoin(p: H :: T, s: L): H :: O = {
  //      p.head :: splitter0.conjoin(p.tail, s)
  //    }

  //    def disjoin(l: H :: O) = {
  //      val (p0, s) = splitter0.disjoin(l.tail)
  //      (l.head :: p0, s)
  //    }
  //  }
  //}

  implicit def splitter[P <: HList, S <: HList, L <: HList, N <: Nat](implicit
      prepend: PrependAux[P, S, L], take: TakeAux[L, N, P], drop: DropAux[L, N, S]) = {
    new Splitter[P, S, L] {
      def conjoin(p: P, s: S): L = prepend(p, s)
      def disjoin(l: L): (P, S) = (take(l), drop(l))
    }
  }
}
