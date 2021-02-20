package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList = IntCons(elem, this)

}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  @tailrec
  def foldLeft(f: Int => Int => Int)(intList: IntList)(init: Int): Int = intList match {
    case IntNil => init
    case _ => foldLeft(f)(intList.tail)(f(intList.head)(init))
  }

  def fromSeq(seq: Seq[Int]): IntList = seq match {
    case _ if seq.isEmpty => IntNil
    case _ => seq.foldRight(IntNil: IntList)((elem: Int, intList: IntList) => IntCons(elem, intList))
  }

  def sum(intList: IntList): Int = intList match {
    case IntNil => undef
    case _ => foldLeft(x => _ + x)(intList)(0)
  }

  def size(intList: IntList): Int = intList match {
    case IntNil => 0
    case _ => 1 + size(intList.tail)
  }

}

case object IntNil extends IntList {
  override def head: Int = undef

  override def tail: IntList = undef

  override def drop(n: Int): IntList = n match {
    case 0 => this
    case _ => undef
  }

  override def take(n: Int): IntList = n match {
    case 0 => this
    case _ => undef
  }

  override def map(f: Int => Int): IntList = this
}

case class IntCons(override val head: Int, override val tail: IntList) extends IntList {
  override def drop(n: Int): IntList = n match {
    case 0 => this
    case _ => tail.drop(n - 1)
  }

  override def take(n: Int): IntList = n match {
    case 0 => IntNil
    case _ => IntCons(head, tail.take(n - 1))

  }

  override def map(f: Int => Int): IntList = IntCons(f(head), tail.map(f))


}

