package org.spbsu.mkn.scala
import org.spbsu.mkn.scala.MyGenericList._
import scala.annotation.tailrec

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[K](f: T => K): MyGenericList[K]
  def ::[K >: T](elem: K): MyGenericList[K] = MyCons(elem, this)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T >: Nothing](seq: Seq[T]): MyGenericList[T] = seq match {
    case Nil => MyNil
    case _ => seq.head :: (this fromSeq seq.tail)
  }
  def size[T](genList: MyGenericList[T]): Int = genList match {
    case MyNil => 0
    case _     => foldLeft(genList)((acc : Int, _) => acc + 1, 0)
  }
  def sum[T <: Int](genList: MyGenericList[T]): Int = genList match {
    case MyNil => undef
    case _     => foldLeft(genList)((acc : Int, x : Int) => acc + x, 0)
  }

  @tailrec
  def foldLeft[K, T](genList: MyGenericList[T])(f: (K, T) => K, init: K): K = genList  match {
      case MyNil => init
      case _     => foldLeft(genList.tail)(f, f(init, genList.head))
    }
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = undef
  override def drop(n: Int): MyGenericList[Nothing] = if (n == 0) this else undef
  override def take(n: Int): MyGenericList[Nothing] = if (n == 0) this else undef
  override def map[K](f: Nothing => K): MyGenericList[K] = this
}

case class MyCons[T](override val head: T, override val tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = n match {
    case i if i < 0 => MyNil
    case 0 => this
    case _ => tail drop (n - 1)
  }
  override def take(n: Int): MyGenericList[T] = n match {
    case i if i <= 0 => MyNil
    case _ => head :: (tail take (n - 1))
  }
  override def map[K](f: T => K): MyGenericList[K] = f(head) :: (tail map f)
}
