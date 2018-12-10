package data

import scalaz._

sealed abstract class ZipMove[A]
case class ZipStop[A](value: A) extends ZipMove[A]
case class ZipLeft[A](next: ZipMove[A]) extends ZipMove[A]
case class ZipRight[A](next: ZipMove[A]) extends ZipMove[A]
