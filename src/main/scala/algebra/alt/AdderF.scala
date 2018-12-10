package algebra.alt

import scalaz._

// This is a Scala port of the free/cofree examples as described in
// http://dlaing.org/cofun/

final case class AddF[A](value: Int, run: Boolean => A)
final case class ClearF[A](next: A)
final case class TotalF[A](run: Int => A)

object AdderF {

  implicit val functorAddF: Functor[AddF] = new Functor[AddF] {
    def map[A, B](fa: AddF[A])(f: A => B) = AddF(fa.value, fa.run andThen f)
  }

  implicit val functorClearF: Functor[ClearF] = new Functor[ClearF] {
    def map[A, B](fa: ClearF[A])(f: A => B) = ClearF(f(fa.next))
  }

  implicit val functorTotalF: Functor[TotalF] = new Functor[TotalF] {
    def map[A, B](fa: TotalF[A])(f: A => B) = TotalF(fa.run andThen f)
  }

  type Adder[A] = Coproduct[AddF, Coproduct[ClearF, TotalF, ?], A]
  type _Add[F[_]] = AddF :<: F
  type _Clear[F[_]] = ClearF :<: F
  type _Total[F[_]] = TotalF :<: F

  def add[F[_]](i: Int)(implicit I: AddF :<: F): Free[F, Boolean] = Free.liftF(I.inj(AddF(i, identity)))
  def clear[F[_]](implicit I: ClearF :<: F): Free[F, Unit] = Free.liftF(I.inj(ClearF(())))
  def total[F[_]](implicit I: TotalF :<: F): Free[F, Int] = Free.liftF(I.inj(TotalF(identity)))

  def findLimit[F[_]: _Add :_Clear :_Total]: Free[F, Int] = for {
    t <- total
    _ <- clear
    r <- findLimit2.run(0)
    _ <- clear
    _ <- add(t)
  } yield r._1

  def findLimit2[F[_]: _Add]: StateT[Int, Free[F, ?], Unit] = for {
    r <- StateT.liftM(add(1))
    r2 <- if (r)
      for {
        _ <- StateT.modify[Free[F, ?], Int]((x: Int) => x + 1)
        r2 <- findLimit2
      } yield r2
    else
      StateT.stateT[Int, Free[F, ?], Boolean](r)
  } yield ()

}