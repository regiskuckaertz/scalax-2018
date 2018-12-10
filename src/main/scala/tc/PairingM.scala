package tc

import data._
import scalaz.{ Compose => _, _ }
import scalaz.Id.Id
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.comonad._
import scalaz.syntax.functor._

trait PairingM[M[_], F[_], G[_]] {
  def pairM[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => M[C]): M[C]
}

object PairingM {
  def apply[M[_], F[_], G[_]](implicit P: PairingM[M, F, G]): PairingM[M, F, G] = P

  def pair[F[_], G[_], A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C)(implicit P: PairingM[Id, F, G]): C =
    P.pairM(fa, gb)(f)

  implicit def free[M[_], F[_]: Functor, G[_]](implicit P: PairingM[M, F, G]) = 
    new PairingM[M, Free[F, ?], Cofree[G, ?]] {
      def pairM[A, B, C](fa: Free[F, A], gb: Cofree[G, B])(f: (A, B) => M[C]) =
        fa.resume match {
          case \/-(a)   => f(a, gb.head)
          case -\/(ffa) => P.pairM(ffa, gb.tail)(pairM(_, _)(f))
        }
    }

  implicit def product[M[_], F1[_], F2[_], G1[_], G2[_]](implicit
    P1: PairingM[M, F1, F2],
    P2: PairingM[M, G1, G2]
  ) = new PairingM[M, Coproduct[F1, G1, ?], Produkt[F2, G2, ?]] {
    def pairM[A, B, C](fa: Coproduct[F1, G1, A], gb: Produkt[F2, G2, B])(f: (A, B) => M[C]) =
      fa.run.fold(P1.pairM(_, gb.fst)(f), P2.pairM(_, gb.snd)(f))
  }
}
