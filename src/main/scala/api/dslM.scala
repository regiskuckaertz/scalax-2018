package api

import scalaz._
import tc._

sealed trait UserApiM[M[_], A]
final case class GetAllUsersM[M[_], A](next: List[User] => M[A]) extends UserApiM[M, A]
final case class GetUserM[M[_], A](id: Int, next: Option[User] => M[A]) extends UserApiM[M, A]
final case class AddUserM[M[_], A](user: User, next: Int => M[A]) extends UserApiM[M, A]

object UserApiM {
  def getAll[F[_], M[_]: Applicative](implicit I: UserApiM[M, ?] :<: F)         : Free[F, List[User]]   = Free.liftF(I.inj(GetAllUsersM(Applicative[M].pure(_))))
  def get[F[_], M[_]: Applicative](id: Int)(implicit I: UserApiM[M, ?] :<: F)   : Free[F, Option[User]] = Free.liftF(I.inj(GetUserM(id, Applicative[M].pure(_))))
  def add[F[_], M[_]: Applicative](user: User)(implicit I: UserApiM[M, ?] :<: F): Free[F, Int]          = Free.liftF(I.inj(AddUserM(user, Applicative[M].pure(_))))

  implicit def functor[M[_]: Functor] = new Functor[UserApiM[M, ?]] {
    val F = Functor[M]

    def map[A, B](fa: UserApiM[M, A])(f: A => B) = fa match {
      case GetAllUsersM(next)   => GetAllUsersM(next.andThen(F.map(_)(f)))
      case GetUserM(id, next)   => GetUserM(id, next.andThen(F.map(_)(f)))
      case AddUserM(user, next) => AddUserM(user, next.andThen(F.map(_)(f)))
    }
  }

}

