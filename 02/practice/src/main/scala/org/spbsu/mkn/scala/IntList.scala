package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList.undef

sealed trait IntList {
  def i: Int
  def head: IntList
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = IntCons(elem, this)
}
case object IntNil extends IntList {
  override def i: Int = undef
  override def head: IntList = undef
  override def tail: IntList = undef
  override def drop(n: Int): IntList = undef
  override def take(n: Int): IntList = undef
  override def map(f: Int => Int): IntList = this
}
case class IntCons(i: Int, tail: IntList) extends IntList {
  override def head: IntList = IntCons(i, IntNil)
  override def drop(n: Int): IntList = this match {
    case IntCons(_, tail) if n > 1  => tail.drop(n-1)
    case IntCons(_, tail) if n == 1 => tail
    case IntCons(_, _)    if n == 0 => this
  }
  override def take(n: Int): IntList = this match {
    case IntCons(i, tail) if n > 1  => IntCons(i, tail.take(n-1))
    case IntCons(i, _)    if n == 1 => IntCons(i, IntNil)
    case IntCons(_, _)    if n == 0 => IntNil
  }
  override def map(f: Int => Int): IntList = IntCons(f(i), tail.map(f))
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight(IntNil: IntList)((i: Int, acc: IntList) => IntCons(i, acc))
  def sum(intList: IntList): Int = ???
  def size(intList: IntList): Int = ???
}
