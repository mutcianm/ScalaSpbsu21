package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList
}

case class FPList(var head: Int, var tail: IntList) extends IntList {
  override def drop(n: Int): IntList = {
    if (n <= 0) {
      return this
    }
    tail.drop(n - 1)
  }

  override def take(n: Int): IntList = {
    if (n <= 0) {
      return IntNil
    }
    tail = tail.take(n - 1)
    this
  }

  override def map(f: Int => Int): IntList = {
    head = f(head)
    tail = tail.map(f)
    this
  }

  override def ::(elem: Int): IntList = {
    tail = this
    head = elem
    this
  }
}

object IntNil extends IntList {
  override def head: Int = {
    throw new UnsupportedOperationException("trying to take non existing element")
  }

  override def tail: IntList = null

  override def drop(n: Int): IntList = {
    if (n != 0) {
      throw new UnsupportedOperationException("trying to drop more elements than exists")
    }
    this
  }

  override def take(n: Int): IntList = {
    if (n > 0) {
      throw new UnsupportedOperationException("trying to take more elements than exists")
    }
    this
  }

  override def map(f: Int => Int): IntList = {
    this
  }

  override def ::(elem: Int): IntList = {
    this
  }
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = {
    if (seq.isEmpty) {
      return IntNil
    }
    FPList(seq.head, fromSeq(seq.tail))
  }

  def sum(intList: IntList): Int = {
    if (intList.equals(IntNil)) throw new UnsupportedOperationException("idk why")
    foldLeft((a, b) => a + b, 0, intList)
  }

  def size(intList: IntList): Int = foldLeft((a, _) => a + 1, 0, intList)

  @tailrec
  def foldLeft(f: (Int, Int) => Int, start: Int, intList: IntList): Int = {
    if (intList.equals(IntNil)) return start
    foldLeft(f, f(start, intList.head), intList.tail)
  }
}
