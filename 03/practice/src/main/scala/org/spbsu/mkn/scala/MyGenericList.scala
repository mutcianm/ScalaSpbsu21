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
  def ::(elem: T): MyGenericList[T]
}

case class MyNil[T]() extends MyGenericList[T] {
  override def head: T = undef
  override def tail: MyGenericList[T] = undef
  override def drop(n: Int): MyGenericList[T] = undef
  override def take(n: Int): MyGenericList[T] = undef
  override def map[K](f: T => K): MyGenericList[K] = MyNil[K]()
  override def ::(elem: T): MyGenericList[T] = MyGenericList(Seq(elem))
}

case class TMore[T](seq: Seq[T]) extends MyGenericList[T] {
  override def head: T = seq.head
  override def tail: MyGenericList[T] = MyGenericList(seq.tail)
  override def drop(n: Int): MyGenericList[T] =
    if (seq.size < n) { undef } else { MyGenericList(seq.drop(n)) }
  override def take(n: Int): MyGenericList[T] =
    if (seq.size < n) { undef } else { MyGenericList(seq.take(n)) }
  override def map[K](f: T => K): MyGenericList[K] = MyGenericList(seq.map(f))
  override def ::(elem: T): MyGenericList[T] = MyGenericList(Seq(elem) ++ seq)
}

object MyGenericList {
  private def sumOfTwoNumbers[T](first: T, second: T)(implicit n: Numeric[T]) : T = {
    import n.mkNumericOps
    first + second
  }
  def apply[T](seq: Seq[T]): MyGenericList[T] =
    if (seq.isEmpty) { MyNil[T]() } else { TMore(seq) }
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