package algebra

import scalaz._

// This is a Scala port of the free/cofree examples as described in
// http://dlaing.org/cofun/

sealed abstract class AdderF[A]
final case class Add[A](value: Int, run: Boolean => A) extends AdderF[A]
final case class Clear[A](next: A) extends AdderF[A]
final case class Total[A](run: Int => A) extends AdderF[A]

object AdderF {
  type Adder[A] = Free[AdderF, A]
  type AdderT[M[_], A] = FreeT[AdderF, M, A]

  implicit val functorAdderF: Functor[AdderF] = new Functor[AdderF] {
    def map[A, B](fa: AdderF[A])(f: A => B) = fa match {
      case Add(x, g) => Add(x, g andThen f)
      case Clear(a) => Clear(f(a))
      case Total(g) => Total(g andThen f)
    }
  }

  def add(i: Int): Adder[Boolean] = Free.liftF(Add(i, identity))
  def clear: Adder[Unit] = Free.liftF(Clear(()))
  def total: Adder[Int] = Free.liftF(Total(identity))

  def findLimit: Adder[Int] = for {
    t <- total
    _ <- clear
    r <- findLimit2.run(0)
    _ <- clear
    _ <- add(t)
  } yield r._1

  def findLimit2: StateT[Int, Adder, Unit] = for {
    r <- StateT.liftM(add(1))
    r2 <- if (r)
      for {
        _ <- StateT.modify[Adder, Int](_ + 1)
        limit <- findLimit2
      } yield limit
    else
      StateT.stateT[Int, Adder, Unit](())
  } yield ()

}