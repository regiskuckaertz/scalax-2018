package api

import scalaz._
import tc._

sealed trait UserApi[A]
final case class GetAllUsers[A](next: List[User] => A) extends UserApi[A]
final case class GetUser[A](id: Int, next: Option[User] => A) extends UserApi[A]
final case class AddUser[A](user: User, next: Int => A) extends UserApi[A]

object UserApi {
  def getAll[F[_]](implicit I: UserApi :<: F)         : Free[F, List[User]]   = Free.liftF(I.inj(GetAllUsers(identity)))
  def get[F[_]](id: Int)(implicit I: UserApi :<: F)   : Free[F, Option[User]] = Free.liftF(I.inj(GetUser(id, identity)))
  def add[F[_]](user: User)(implicit I: UserApi :<: F): Free[F, Int]          = Free.liftF(I.inj(AddUser(user, identity)))

  implicit val functor = new Functor[UserApi] {
    def map[A, B](fa: UserApi[A])(f: A => B) = fa match {
      case GetAllUsers(next)   => GetAllUsers(next andThen f)
      case GetUser(id, next)   => GetUser(id, next andThen f)
      case AddUser(user, next) => AddUser(user, next andThen f)
    }
  }

  def eval[F[_], A, B](program: Free[UserApi, A], interp: Cofree[F, B])(implicit P: Free[UserApi, ?] ⋈ Cofree[F, ?]): A =
    P.pair(program, interp)((a, _) => a)
}

sealed trait ProductApi[A]
final case class GetAllProducts[A](next: List[Product] => A) extends ProductApi[A]
final case class GetProduct[A](id: Int, next: Option[Product] => A) extends ProductApi[A]
final case class AddProduct[A](product: Product, next: Int => A) extends ProductApi[A]

object ProductApi {
  def getAll[F[_]](implicit I: ProductApi :<: F): Free[F, List[Product]] = Free.liftF(I.inj(GetAllProducts(identity)))
  def get[F[_]](id: Int)(implicit I: ProductApi :<: F): Free[F, Option[Product]] = Free.liftF(I.inj(GetProduct(id, identity)))
  def add[F[_]](prod: Product)(implicit I: ProductApi :<: F): Free[F, Int] = Free.liftF(I.inj(AddProduct(prod, identity)))

  implicit val functor = new Functor[ProductApi] {
    def map[A, B](fa: ProductApi[A])(f: A => B) = fa match {
      case GetAllProducts(next)      => GetAllProducts(next andThen f)
      case GetProduct(id, next)      => GetProduct(id, next andThen f)
      case AddProduct(product, next) => AddProduct(product, next andThen f)
    }
  }

  def eval[F[_], A, B](program: Free[ProductApi, A], interp: Cofree[F, B])(implicit P: Free[ProductApi, ?] ⋈ Cofree[F, ?]): A =
    P.pair(program, interp)((a, _) => a)
}
