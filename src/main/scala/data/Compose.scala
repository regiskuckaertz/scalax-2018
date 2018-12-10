package data

import scalaz.Functor
import scalaz.syntax.functor._

final case class Compose[F[_], G[_], A](run: F[G[A]])

object Compose {

  implicit def functor[F[_]: Functor, G[_]: Functor]: Functor[Compose[F, G, ?]] = new Functor[Compose[F, G, ?]] {
    def map[A, B](fa: Compose[F, G, A])(f: A => B) = Compose(fa.run.map(_.map(f)))
  }

}