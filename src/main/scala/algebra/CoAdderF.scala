package algebra

import scalaz._
import tc._
import AdderF._

// This is a Scala port of the free/cofree examples as described in
// http://dlaing.org/cofun/

final case class CoAdderF[A](
  addH : Int => (Boolean, A),
  clearH : A,
  totalH : (Int, A)
)

object CoAdderF {
  type CoAdder[A] = Cofree[CoAdderF, A]

  implicit val functorCoAdderF: Functor[CoAdderF] = new Functor[CoAdderF] {
    def map[A, B](fa: CoAdderF[A])(f: A => B) = CoAdderF(
      { x => val (b, a) = fa.addH(x); (b, f(a)) },
      f(fa.clearH),
      (fa.totalH._1, f(fa.totalH._2))
    )
  }

  type Count = Int
  type Limit = Int

  def coAdd(w: (Limit, Count))(x: Int): (Boolean, (Limit, Count)) = {
    val count = w._2 + x
    val test = count <= w._1
    val next = if (test) count else w._2
    (test, (w._1, next))
  }

  def coClear(w: (Limit, Count)): (Limit, Count) = (w._1, 0)

  def coTotal(w: (Limit, Count)): (Int, (Limit, Count)) = (w._2, w)
  
  def mkCoAdder(limit: Limit, count: Count): CoAdder[(Limit, Count)] = {
    val start = (limit, count)
    def next(w: (Limit, Count)): CoAdderF[(Limit, Count)] =
      CoAdderF(coAdd(w), coClear(w), coTotal(w))
    Cofree.unfoldC(start)(next)
  }

  implicit val pairingAdderCoAdder: Pairing[AdderF, CoAdderF] = new Pairing[AdderF, CoAdderF] {
    def pair[A, B, R](fa: AdderF[A], gb: CoAdderF[B])(f: (A, B) => R) = fa match {
      case Add(value, run) => 
        val (test, b) = gb.addH(value)
        f(run(test), b)
      case Clear(next) => 
        f(next, gb.clearH)
      case Total(run)  =>
        f(run(gb.totalH._1), gb.totalH._2)
    }
  }

  def runLimit[A](w: CoAdder[A]): Int =
    Pairing[Adder, CoAdder].pair(findLimit, w)((a, _) => a)

  def testLimit(x: Int): Boolean =
    runLimit(mkCoAdder(x, 0)) == x
  
  // def consoleAdder0: FreeT[AdderF, IO, Unit] = {
  //   val prompt = """Commands:
  //                  |  add [int]
  //                  |  clear
  //                  |  total
  //                  |""".stripMargin

  //   for {
  //     l <- getLine
  //     l.split(' ') match {
  //       case List("add" :: x) => add (read x) >>= \b ->
  //         putStrLn("add result: " ++ show b)
  //       case List("clear") => clear
  //       case List("total") => total >>= \t ->
  //         putStrLn("total result: " ++ show t)
  //       case _ => putStrLn(prompt)
  //     }
  //   } yield ()
  // }

  // def consoleAdder: FreeT[AdderF, IO, Unit] =
  //   consoleAdder0.forever
}