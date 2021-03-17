package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sum, sort}

class MyGenericListTest extends AnyFunSuite {


  test("head") {
    assert(fromSeq[Int](Seq(1, 2, 3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1, 2, 3)).tail == fromSeq(Seq(2, 3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1, 2, 3)).drop(0) == fromSeq(Seq(1, 2, 3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1, 2, 3)).take(0) == MyNil)
    assert(fromSeq(Seq(1, 2, 3)).take(2) == fromSeq(Seq(1, 2)))
    assert(fromSeq(Seq(1, 2, 3)).take(3) == fromSeq(Seq(1, 2, 3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).take(10))
  }

  test("map") {
    assert(MyNil.map((x: Int) => x * 2) == MyNil)
    assert(fromSeq(Seq(1, 2, 3)).map(_ * 2) == fromSeq(Seq(2, 4, 6)))
    assert(fromSeq(Seq(1, 2, 3)).map(identity) == fromSeq(Seq(1, 2, 3)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1, 2, 3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(MyNil))
    assert(sum(fromSeq(Seq(1, 2, 3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }
  test("sort") {
    assert(sort(MyNil: MyGenericList[Int]) == MyNil)
    assert(sort(fromSeq(Seq(0))) == fromSeq(Seq(0)))
    assert(sort(fromSeq(Seq(3, 4, 5, 6, 1, 2))) == fromSeq(Seq(1, 2, 3, 4, 5, 6)))
    assert(sort(fromSeq(Seq("aaa", "c", "bb"))) == fromSeq(Seq("aaa", "bb", "c")))
  }

}
