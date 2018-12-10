package tc

import data._
import scalaz.{ Compose => _, _ }
import scalaz.Id.Id
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.comonad._
import scalaz.syntax.functor._

trait Pairing[F[_], G[_]] {
  def pair[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C): C
}

object Pairing extends PairingInstances {
  def apply[F[_], G[_]](implicit P: F ⋈ G): F ⋈ G = P

  def zap[F[_], G[_], A, B](fab: F[A => B], ga: G[A])(implicit P: F ⋈ G): B =
    P.pair(fab, ga)(_(_))

  def moving[W[_]: Comonad, M[_], A, B](wa: W[A], mab: M[A => B])(implicit P: W ⋈ M): W[B] =
    P.pair(wa.cojoin, mab)((wa, ab) => Functor[W].map(wa)(ab))

  def move[W[_]: Comonad, M[_]: Functor, A](wa: W[A], mu: M[Unit])(implicit P: W ⋈ M): W[A] =
    moving(wa, Functor[M].map(mu)(_ => identity((a: A) => a)))

  def select[W[_], M[_], A, B](wa: W[W[A]], mb: M[B])(implicit P: W ⋈ M): W[A] =
    P.pair(wa, mb)((wa, _) => wa)
}

trait PairingInstances {
  implicit val id = new Pairing[Id, Id] {
    def pair[A, B, C](fa: Id[A], gb: Id[B])(f: (A, B) => C) = 
      f(fa, gb)
  }

  implicit def curry[X] = new Pairing[X => ?, (X, ?)] {
    def pair[A, B, C](fa: X => A, gb: (X, B))(f: (A, B) => C) =
      f(fa(gb._1), gb._2)
  }

  implicit def cocurry[X] = new Pairing[(X, ?), X => ?] {
    def pair[A, B, C](fa: (X, A), gb: X => B)(f: (A, B) => C) =
      f(fa._2, gb(fa._1))
  }

  implicit def store[F[_], G[_]: Bind, S](implicit P: F ⋈ G) = new Pairing[StoreT[F, S, ?], StateT[S, G, ?]] {
    def pair[A, B, C](fa: StoreT[F, S, A], gb: StateT[S, G, B])(f: (A, B) => C) =
      P.pair(fa.set, gb.run(fa.pos))((f1, s) => f(f1(s._1), s._2))
  }

  implicit def traced[F[_], G[_], W](implicit P: F ⋈ G) = new Pairing[TracedT[F, W, ?], WriterT[W, G, ?]] {
    def pair[A, B, C](fa: TracedT[F, W, A], gb: WriterT[W, G, B])(f: (A, B) => C) =
      P.pair(fa.run, gb.run)((f1, wa) => f(f1(wa._1), wa._2))
  }

  implicit val zipper: Pairing[ZipMove, Zipper] = new Pairing[ZipMove, Zipper] {
    def pair[A, B, C](fa: ZipMove[A], gb: Zipper[B])(f: (A, B) => C) = fa match {
      case ZipStop(a)     => f(a, gb.focus)
      case ZipLeft(next)  => pair(next, gb.previousC)(f)
      case ZipRight(next) => pair(next, gb.nextC)(f)
    }
  }

  implicit def free[F[_]: Functor, G[_]](implicit P: F ⋈ G) = 
    new Pairing[Free[F, ?], Cofree[G, ?]] {
      def pair[A, B, C](fa: Free[F, A], gb: Cofree[G, B])(f: (A, B) => C) =
        fa.resume match {
          case \/-(a) => f(a, gb.head)
          case -\/(ffa) => P.pair(ffa, gb.tail)(pair(_, _)(f))
        }
    }

  implicit def compose[F1[_], G1[_], F2[_], G2[_]](implicit P1: F1 ⋈ F2, P2: G1 ⋈ G2) = 
    new Pairing[Compose[F1, G1, ?], Compose[F2, G2, ?]] {
      def pair[A, B, C](fa: Compose[F1, G1, A], gb: Compose[F2, G2, B])(f: (A, B) => C) =
        P1.pair(fa.run, gb.run)(P2.pair(_, _)(f))
    }

  implicit def product[F1[_], F2[_], G1[_], G2[_]](implicit P1: F1 ⋈ F2, P2: G1 ⋈ G2) = 
    new Pairing[Coproduct[F1, G1, ?], Produkt[F2, G2, ?]] {
      def pair[A, B, C](fa: Coproduct[F1, G1, A], gb: Produkt[F2, G2, B])(f: (A, B) => C) =  
        fa.run.fold(P1.pair(_, gb.fst)(f), P2.pair(_, gb.snd)(f))
    }
}
