package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._
import scala.annotation.tailrec

sealed trait MyGenericList[+T] {
  def head: T

  def tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[A](f: T => A): MyGenericList[A]

  def ::[A >: T](elem: A): MyGenericList[A] = MyCons(elem, this)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  @tailrec
  def foldLeft[T, A](f: T => A => A)(genList: MyGenericList[T])(init: A): A = genList match {
    case MyNil => init
    case _ => foldLeft(f)(genList.tail)(f(genList.head)(init))
  }

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = seq match {
    case _ if seq.isEmpty => MyNil
    case _ => seq.foldRight(MyNil: MyGenericList[T])((elem: T, genList: MyGenericList[T]) => elem :: genList)
  }

  def sum[T <: Int](genList: MyGenericList[T]): Int = genList match {
    case MyNil => undef
    case _ => foldLeft[T, Int](x => y => x + y)(genList)(0)
  }

  def size[T](genList: MyGenericList[T]): Int = genList match {
    case MyNil => 0
    case _ => foldLeft[T, Int](_ => x => x + 1)(genList)(0)
  }


  def sort[T](list: MyGenericList[T])(implicit comparator: Ordering[T]): MyGenericList[T] = list match {
    case MyNil => MyNil
    case notNil =>
      val newListToSort = sort(notNil.tail)
      findRightPlace(notNil.head, newListToSort)
  }

  private def findRightPlace[T](elem: T, list: MyGenericList[T])(implicit comparator: Ordering[T]): MyGenericList[T] = list match {
    case MyNil => MyCons(elem, MyNil)
    case notNil =>
      if (comparator.gteq(elem, notNil.head))
        MyCons(notNil.head, findRightPlace(elem, notNil.tail))
      else
        MyCons(elem, notNil)
  }


}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = n match {
    case 0 => this
    case _ => undef
  }

  override def take(n: Int): MyGenericList[Nothing] = n match {
    case 0 => this
    case _ => undef
  }

  override def map[A](f: Nothing => A): MyGenericList[A] = MyNil
}

case class MyCons[T](override val head: T, override val tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = n match {
    case 0 => this
    case _ => tail.drop(n - 1)
  }

  override def take(n: Int): MyGenericList[T] = n match {
    case 0 => MyNil
    case _ => MyCons(head, tail.take(n - 1))

  }

  override def map[A](f: T => A): MyGenericList[A] = MyCons(f(head), tail.map(f))

}