package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._
import scala.annotation.tailrec
import scala.Numeric

sealed trait MyGenericList[T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[K](f: T => K): MyGenericList[K]
  def ::(elem: T): MyGenericList[T] = TMore(elem, this)
}

case class MyNil[T]() extends MyGenericList[T] {
  override def head: T = undef
  override def tail: MyGenericList[T] = undef
  override def drop(n: Int): MyGenericList[T] = if (n == 0) { this } else { undef }
  override def take(n: Int): MyGenericList[T] = if (n == 0) { this } else { undef }
  override def map[K](f: T => K): MyGenericList[K] = MyNil[K]()
}

case class TMore[T](value: T, prev: MyGenericList[T]) extends MyGenericList[T] {
  override def head: T = value
  override def tail: MyGenericList[T] = prev
  override def drop(n: Int): MyGenericList[T] =
    if (n == 0) { this } else { prev.drop(n - 1) }
  override def take(n: Int): MyGenericList[T] =
    if (n == 0) { MyNil() } else { TMore(value, prev.take(n - 1)) }
  override def map[K](f: T => K): MyGenericList[K] = TMore(f(value), prev.map(f))
}

object MyGenericList {
  private def sumOfTwoNumbers[T](first: T, second: T)(implicit n: Numeric[T]) : T = {
    import n.mkNumericOps
    first + second
  }
  def apply[T](seq: Seq[T]): MyGenericList[T] =
    if (seq.isEmpty) { MyNil() } else { TMore(seq.head, MyGenericList(seq.tail)) }
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = MyGenericList(seq)
  def size[T](TList: MyGenericList[T]): Int = foldLeft((currentCount: Int, now: T) => currentCount + 1, 0, TList)
  def sum[T](TList: MyGenericList[T])(implicit n: Numeric[T]): T =
    TList match {
      case MyNil() => undef
      case _ => foldLeft((currentSum: T, now : T) => sumOfTwoNumbers(now, currentSum), TList.head, TList.tail) // Scala is broken
    }
  @tailrec
  def foldLeft[T, K](f: (K, T) => K, startValue: K, TList: MyGenericList[T]): K = {
    TList match {
      case MyNil() => startValue
      case _ =>
        foldLeft(f, f(startValue, TList.head), TList.tail)
    }
  }
}