package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList

  def foldLeft[B](init: B)(op: (Int, B) => B): B

  def ::(elem: Int): IntList = IntListBuilder(elem, this)
}

case object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight(IntNil: IntList)((cur, total) => cur :: total)

  def sum(intList: IntList): Int = intList match {
    case IntListBuilder(x, IntNil) => x
    case IntListBuilder(x, lst) => x + sum(lst)
    case _ => undef
  }

  def size(intList: IntList): Int = intList.foldLeft(0)((_, cnt) => cnt + 1)
}

case object IntNil extends IntList {
  override def head: Int = undef

  override def tail: IntList = undef

  override def drop(n: Int): IntList = n match {
    case 0 => IntNil
    case x => undef
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case x => undef
  }

  override def map(f: Int => Int): IntList = IntNil

  override def ::(elem: Int): IntList = IntListBuilder(elem, IntNil)

  override def foldLeft[B](init: B)(op: (Int, B) => B): B = init
}

case class IntListBuilder(elem: Int, list: IntList) extends IntList {
  override val head: Int = elem

  override val tail: IntList = list

  override def drop(n: Int): IntList = n match {
    case 0 => this
    case x if x > 0 => tail.drop(x - 1)
    case _ => undef
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case x if x > 0 => head :: tail.take(x - 1)
    case _ => undef
  }

  override def map(f: Int => Int): IntList = f(head) :: tail.map(f)

  override def foldLeft[B](init: B)(op: (Int, B) => B): B = this match {
    case IntListBuilder(x, IntNil) => op(x, init)
    case IntListBuilder(x, lst) => tail.foldLeft(op(x, init))(op)
  }
}