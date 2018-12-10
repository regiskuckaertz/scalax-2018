package api

import data.Produkt
import scalaz._
import scalaz.syntax.apply._
import scalaz.syntax.functor._
import scalaz.zio.IO
import tc._
import interop.scalaz73._

final case class CoUserApiM[M[_], A](
  getAll: M[(List[User], A)],
  get: Int => M[(Option[User], A)],
  add: User => M[(Int, A)]
)

object CoUserApiM {
  implicit def functorM[M[_]: Functor] = new Functor[CoUserApiM[M, ?]] {
    val F = Functor[M]

    def map[A, B](fa: CoUserApiM[M, A])(f: A => B) = CoUserApiM(
      F.map(fa.getAll) { case (us, a) => (us, f(a)) },
      fa.get.andThen(F.map(_){ case (ou, a) => (ou, f(a)) }),
      fa.add.andThen(F.map(_) { case (uid, a) => (uid, f(a)) })
    )
  }

  implicit def pairingM[M[_]](implicit M: Monad[M]) = new PairingM[M, UserApiM[M, ?], CoUserApiM[M, ?]] {
    def pairM[A, B, C](fa: UserApiM[M, A], gb: CoUserApiM[M, B])(f: (A, B) => M[C]): M[C] =
      fa match {
        case GetAllUsersM(next) =>
          M.bind(gb.getAll) { case (us, b) => M.bind(next(us))(f(_, b)) }
        case GetUserM(id, next) =>
          M.bind(gb.get(id)) { case (ou, b) => M.bind(next(ou))(f(_, b)) }
        case AddUserM(user, next) =>
          M.bind(gb.add(user)) { case (uid, b) => M.bind(next(uid))(f(_, b)) }
      }
  }
}

object TestInterpreterIO {
  type State = (Map[Int, User], Int)

  def mkCoUserApi(state: State): CoUserApiM[IO[Nothing, ?], State] = CoUserApiM(
    IO.sync((state._1.values.toList, state)),
    id => IO.sync((state._1.get(id), state)),
    u => IO.sync {
      val uid = state._2 + 1
      val users = state._1 + (uid -> u)
      (uid, (users, uid))
    }
  )

  val interpreter = Cofree.unfoldC[CoUserApiM[IO[Nothing, ?], ?], State]((Map.empty, 0))(mkCoUserApi)
}

