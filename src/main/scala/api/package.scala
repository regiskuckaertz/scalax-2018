import scalaz._
import scalaz.zio.IO
import tc._
import interop.scalaz73._

package object api {

  type Program[A] = Coproduct[UserApi, ProductApi, A]

  val testP: Free[Program, (List[User], List[Product])] = for {
    _ <- UserApi.add[Program](User(0, "Regis", "Kuckaertz"))
    _ <- ProductApi.add[Program](Product(0, "Laptop sleeve"))
    _ <- UserApi.add[Program](User(0, "John", "de Goes"))
    _ <- ProductApi.add[Program](Product(0, "Macbook pro"))
    us <- UserApi.getAll[Program]
    ps <- ProductApi.getAll[Program]
  } yield (us, ps)

  def eval[F[_], A, B](program: Free[Program, A], interp: Cofree[F, B])(implicit P: Free[Program, ?] ⋈ Cofree[F, ?]): A =
    P.pair(program, interp)((a, _) => a)


  val testM: Free[UserApiM[IO[Nothing, ?], ?], List[User]] = for {
    _ <- UserApiM.add[UserApiM[IO[Nothing, ?], ?], IO[Nothing, ?]](User(0, "Regis", "Kuckaertz"))
    _ <- UserApiM.add[UserApiM[IO[Nothing, ?], ?], IO[Nothing, ?]](User(0, "John", "de Goes"))
    us <- UserApiM.getAll[UserApiM[IO[Nothing, ?], ?], IO[Nothing, ?]]
  } yield us

  def evalM[M[_]: Monad, F[_], A, B](program: Free[UserApiM[M, ?], A], interp: Cofree[F, B])(implicit P: PairingM[M, Free[UserApiM[M, ?], ?], Cofree[F, ?]]) =
    P.pairM(program, interp)((a, _) => Monad[M].pure(a))

}