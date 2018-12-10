package algebra.alt

import scalaz.{ Functor, Free, Cofree }
import tc._
import data._
import AdderF._

// This is a Scala port of the free/cofree examples as described in
// http://dlaing.org/cofun/

final case class CoAddF[A](run: Int => (Boolean, A))
final case class CoClearF[A](value: A)
final case class CoTotalF[A](value: (Int, A))

object CoAdderF {
  implicit val functorCoAddF: Functor[CoAddF] = new Functor[CoAddF] {
    def map[A, B](fa: CoAddF[A])(f: A => B) = 
      CoAddF({x => val (b, a) = fa.run(x); (b, f(a))})
  }

  implicit val functorCoClearF: Functor[CoClearF] = new Functor[CoClearF] {
    def map[A, B](fa: CoClearF[A])(f: A => B) = 
      CoClearF(f(fa.value))
  }

  implicit val functorCoTotalF: Functor[CoTotalF] = new Functor[CoTotalF] {
    def map[A, B](fa: CoTotalF[A])(f: A => B) = 
      CoTotalF((fa.value._1, f(fa.value._2)))
  }

  type CoAdder[A] = Produkt[CoAddF, Produkt[CoClearF, CoTotalF, ?], A]

  type Count = Int
  type Limit = Int

  def coAdd(w: (Limit, Count)): CoAddF[(Limit, Count)] = CoAddF { x =>
    val count = w._2 + x
    val test = count <= w._1
    val next = if (test) count else w._2
    (test, (w._1, next))
  }

  def coClear(w: (Limit, Count)): CoClearF[(Limit, Count)] = CoClearF((w._1, 0))

  def coTotal(w: (Limit, Count)): CoTotalF[(Limit, Count)] = CoTotalF((w._2, w))
  
  def mkCoAdder(limit: Limit, count: Count): Cofree[CoAdder, (Limit, Count)] = {
    val start = (limit, count)
    def next(w: (Limit, Count)): CoAdder[(Limit, Count)] =
      Produkt(coAdd(w), Produkt(coClear(w), coTotal(w)))
    Cofree.unfoldC(start)(next)
  }

  implicit val pairingAdd: Pairing[AddF, CoAddF] = new Pairing[AddF, CoAddF] {
    def pair[A, B, R](fa: AddF[A], gb: CoAddF[B])(f: (A, B) => R) = {
      val (test, b) = gb.run(fa.value)
      f(fa.run(test), b)
    }
  }

  implicit val pairingClear: Pairing[ClearF, CoClearF] = new Pairing[ClearF, CoClearF] {
    def pair[A, B, R](fa: ClearF[A], gb: CoClearF[B])(f: (A, B) => R) =
      f(fa.next, gb.value)
  }

  implicit val pairingTotal: Pairing[TotalF, CoTotalF] = new Pairing[TotalF, CoTotalF] {
    def pair[A, B, R](fa: TotalF[A], gb: CoTotalF[B])(f: (A, B) => R) = {
      val (total, b) = gb.value
      f(fa.run(total), b)
    }
  }

  def runLimit[A](w: Cofree[CoAdder, A]): Int =
    Pairing[Free[Adder, ?], Cofree[CoAdder, ?]].pair(findLimit, w)((a, _) => a)

  def testLimit(x: Int): Boolean =
    runLimit(mkCoAdder(x, 0)) == x
}