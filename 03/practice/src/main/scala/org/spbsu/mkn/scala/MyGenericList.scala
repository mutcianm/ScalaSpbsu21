package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._


sealed trait MyGenericList[+T >: Nothing] {
  val head: T

  val tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[A, NT](f: A => NT): MyGenericList[NT]

  def foldLeft[B](init: B)(op: (T, B) => B): B

  def ::[NT >: T](elem: NT): MyGenericList[NT] = GenericListBuilder(elem, this)
}

case object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): MyGenericList[A] =
    seq.foldRight(MyNil: MyGenericList[A])((cur, total) => cur :: total)

  def sum[A: Numeric](list: MyGenericList[A]): A = {
    val numeric = implicitly[Numeric[A]]
    list match {
      case GenericListBuilder(x, MyNil) => x
      case GenericListBuilder(x, lst) => numeric.plus(x, sum(lst))
      case _ => undef
    }
  }

  def size[A](intList: MyGenericList[A]): Int = intList.foldLeft(0)((_, cnt) => cnt + 1)
}


case class GenericListBuilder[T](elem: T, list: MyGenericList[T]) extends MyGenericList[T] {
  override val head: T = elem

  override val tail: MyGenericList[T] = list

  override def drop(n: Int): MyGenericList[T] = n match {
    case 0 => this
    case x if x > 0 => tail.drop(x - 1)
    case _ => undef
  }

  override def take(n: Int): MyGenericList[T] = n match {
    case 0 => MyNil
    case x if x > 0 => head :: tail.take(x - 1)
    case _ => undef
  }


  override def map[T >: Nothing, NT >: Nothing](f: T => NT): MyGenericList[NT] = f(head) :: tail.map(f)

  override def foldLeft[B](init: B)(op: (T, B) => B): B = this match {
    case GenericListBuilder(x, MyNil) => op(x, init)
    case GenericListBuilder(x, lst) => lst.foldLeft(op(x, init))(op)
  }
}

case object MyNil extends MyGenericList[Nothing] {
  override val head: Nothing = undef

  override val tail: Nothing = undef

  override def drop(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyNil
    case _ => undef
  }

  override def take(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyNil
    case _ => undef
  }

  override def map[A, NT](f: A => NT): MyGenericList[Nothing] = MyNil

  override def foldLeft[B](init: B)(op: (Nothing, B) => B): B = init

  override def ::[NT >: Nothing](elem: NT): MyGenericList[NT] = GenericListBuilder(elem, MyNil)
}
