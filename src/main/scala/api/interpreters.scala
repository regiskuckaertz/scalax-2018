package api

import data.Produkt
import scalaz._
import scalaz.syntax.apply._
import scalaz.syntax.functor._
import scalaz.zio.IO
import tc._

final case class CoUserApi[A](
  getAll: (List[User], A),
  get: Int => (Option[User], A),
  add: User => (Int, A)
)

object CoUserApi {
  implicit val functor = new Functor[CoUserApi] {
    def map[A, B](fa: CoUserApi[A])(f: A => B) = CoUserApi(
      (fa.getAll._1, f(fa.getAll._2)),
      fa.get andThen { case (ou, a) => (ou, f(a)) },
      fa.add andThen { case (uid, a) => (uid, f(a)) }
    )
  }

  implicit val pairing = new Pairing[UserApi, CoUserApi] {
    def pair[A, B, C](fa: UserApi[A], gb: CoUserApi[B])(f: (A, B) => C) = fa match {
      case GetAllUsers(next) =>
        val (users, b) = gb.getAll
        f(next(users), b)
      case GetUser(uid, next) => 
        val (ou, b) = gb.get(uid)
        f(next(ou), b)
      case AddUser(u, next) =>
        val (uid, b) = gb.add(u)
        f(next(uid), b)
    }
  }
}

final case class CoProductApi[A](
  getAll: (List[Product], A),
  get: Int => (Option[Product], A),
  add: Product => (Int, A)
)

object CoProductApi {
  implicit val functor = new Functor[CoProductApi] {
    def map[A, B](fa: CoProductApi[A])(f: A => B) = CoProductApi(
      (fa.getAll._1, f(fa.getAll._2)),
      fa.get andThen { case (ou, a) => (ou, f(a)) },
      fa.add andThen { case (uid, a) => (uid, f(a)) }
    )
  }

  implicit val pairing = new Pairing[ProductApi, CoProductApi] {
    def pair[A, B, C](fa: ProductApi[A], gb: CoProductApi[B])(f: (A, B) => C) = fa match {
      case GetAllProducts(next) =>
        val (products, b) = gb.getAll
        f(next(products), b)
      case GetProduct(uid, next) => 
        val (ou, b) = gb.get(uid)
        f(next(ou), b)
      case AddProduct(u, next) =>
        val (uid, b) = gb.add(u)
        f(next(uid), b)
    }
  }
}

object TestInterpreter {
  type State = (Map[Int, User], Map[Int, Product], Int, Int)
  type Interpreter[A] = Produkt[Cofree[CoUserApi, ?], Cofree[CoProductApi, ?], A]

  def mkCoUserApi(state: State): CoUserApi[State] = CoUserApi(
    (state._1.values.toList, state),
    id => (state._1.get(id), state),
    u => {
      val uid = state._3 + 1
      val users = state._1 + (uid -> u)
      (uid, (users, state._2, uid, state._4))
    }
  )

  def mkCoProductApi(state: State): CoProductApi[State] = CoProductApi(
    (state._2.values.toList, state),
    id => (state._2.get(id), state),
    p => {
      val pid = state._4 + 1
      val products = state._2 + (pid -> p)
      (pid, (state._1, products, state._3, pid))
    }
  )

  def mkInterpreter(state: State): Produkt[CoUserApi, CoProductApi, State] =
    Produkt(mkCoUserApi(state), mkCoProductApi(state))

  def apply: Cofree[Produkt[CoUserApi, CoProductApi, ?], State] =
    Cofree.unfoldC[Produkt[CoUserApi, CoProductApi, ?], State]((Map.empty, Map.empty, 0, 0))(mkInterpreter)

}

