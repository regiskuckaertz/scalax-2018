package data

import scalaz.Functor
import scalaz.syntax.functor._

final case class Produkt[F[_], G[_], A](fst: F[A], snd: G[A])

object Produkt {

  implicit def functorProdukt[F[_]: Functor, G[_]: Functor]: Functor[Produkt[F, G, ?]] =
    new Functor[Produkt[F, G, ?]] {
      def map[A, B](fa: Produkt[F, G, A])(f: A => B) = Produkt(fa.fst.map(f), fa.snd.map(f))
    }

}