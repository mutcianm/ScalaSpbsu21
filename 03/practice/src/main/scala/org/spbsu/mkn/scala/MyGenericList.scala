package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._


sealed trait MyGenericList[+T >: Nothing] {
  def head: T

  def tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[NT >: T](f: NT => NT): MyGenericList[NT]

  def foldLeft[B](init: B)(op: (T, B) => B): B

  def ::[NT >: T](elem: NT): MyGenericList[NT] = GenericListBuilder(elem, this)
}

case object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T >: Nothing](seq: Seq[T]): MyGenericList[T] = seq match {
    case x :: xs => GenericListBuilder(x, fromSeq(xs))
    case _ => MyNil
  }
    //seq.foldRight(MyNil: MyGenericList[T])(GenericListBuilder[T])

  def sum[A <: Int](list: MyGenericList[A]): Int = {
    list match {
      case GenericListBuilder(x, MyNil) => x
      case GenericListBuilder(x, lst) => x + sum(lst)
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


  override def ::[NT >: T](elem: NT): MyGenericList[NT] = GenericListBuilder(elem, this)

  override def foldLeft[B](init: B)(op: (T, B) => B): B = this match {
    case GenericListBuilder(x, MyNil) => op(x, init)
    case GenericListBuilder(x, lst) => lst.foldLeft(op(x, init))(op)
  }

  override def map[NT >: T](f: NT => NT): MyGenericList[NT] = f(head) :: tail.map(f)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: Nothing = undef

  override def drop(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyNil
    case _ => undef
  }

  override def take(n: Int): MyGenericList[Nothing] = n match {
    case 0 => MyNil
    case _ => undef
  }

  override def map[NT >: Nothing](f: NT => NT): MyGenericList[Nothing] = MyNil

  override def foldLeft[B](init: B)(op: (Nothing, B) => B): B = init

  override def ::[NT >: Nothing](elem: NT): MyGenericList[NT] = GenericListBuilder(elem, MyNil)
}
