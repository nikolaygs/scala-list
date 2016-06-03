package com.ngs.list

import scala.annotation.tailrec
import scala.io.Source

sealed abstract class NList[+T] {
  // basic API methods
  def isEmpty: Boolean
  def head: T
  def tail: NList[T]

  //
  // Combinator library
  //
  def foldLeft[X](onEmpty: X)(f: (=> X, T) => X): X = {
    def foldLeftReversed[X](list: NList[T])(onEmpty: X)(f: (=> X, T) => X): X = list match {
      case Empty => onEmpty
      case x :: xs => f(foldLeftReversed(xs)(onEmpty)(f), x)
    }

    foldLeftReversed(this.reverse)(onEmpty)(f)
  }

  def foldRight[X](onEmpty: X)(f: (T, => X) => X): X = this match {
    case Empty => onEmpty
    case x :: xs => f(x, xs.foldRight(onEmpty)(f))
  }

  def filter(f: T => Boolean) = foldRight(Empty.asInstanceOf[NList[T]]) {
    (x, y) =>
      if (f(x)) x :: y
      else y
  }

  def map[B >: T](f: T => B) = foldRight(Empty.asInstanceOf[NList[B]]) {
    (x, y) => f(x) :: y
  }

  def flatMap[B >: T](f: T => NList[B]) = foldRight(Empty.asInstanceOf[NList[B]]) {
    (x, y) => f(x) ::: y
  }

  def size = foldRight(0) {
    (x, y) => y + 1
  }

  def contains[T](elem: T) = foldRight(false) {
    (x, y) => y == elem
  }

  def forall[B >: T](f: B => Boolean) = foldRight(false) {
    (x, y) => f(x) && y
  }

  def exists(f: T => Boolean) = foldRight(false) {
    (x, y) => f(x) || y
  }

  def count(f: T => Boolean) = foldRight(0) {
    (x, y) => if (f(x)) 1 + y else 0
  }

  def foreach(f: T => Unit) = foldRight(()) {
    (x, y) => { f(x); y }
  }

  def reverse = {
    def <---[T](result: NList[T], list: NList[T]): NList[T] = list match {
      case Empty => result
      case x :: xs => <---(x :: result, xs)
    }

    <---(Empty, this)
  }

  def :::[B >: T](that: NList[B]): NList[B] = this match {
    case Empty => that
    case _ => {
      foldRight(Empty.asInstanceOf[NList[B]]) {
        def ++(list: NList[B], result: NList[B]): NList[B] = list match {
          case Empty => result
          case x :: xs => ++(xs, x :: result)
        }

        (x, y) => ++(that.reverse, this)
      }
    }
  }

  //
  // Operator aliases
  //
  def ::[B >: T](head: B) = new ::(head, this)
  def /:[X](onEmpty: X)(f: (=> X, T) => X): X = foldLeft(onEmpty)(f)
  def \:[X](onEmpty: X)(f: (T, => X) => X): X = foldRight(onEmpty)(f)

  override def toString = this match {
    case Empty => "List()"
    case x :: xs => xs.foldLeft("List(" + head)(_ + ", " + _) + ")"
  }

}

object NList {
  def apply[T](t: T*) = {

    @tailrec
    def toNList(iter: Iterator[T], result: NList[T]): NList[T] = {
      if (!iter.hasNext) result
      else toNList(iter, iter.next :: result)
    }

    val iter = t.iterator
    val initial = Empty
    toNList(iter, initial)
  }
}

case object Empty extends NList[Nothing] {
  def isEmpty = true
  def head = throw new IllegalStateException("Empty list do not have head")
  def tail = throw new IllegalStateException("Empty list do not have tail")
}

case class ::[B](head: B, tail: NList[B]) extends NList[B] {
  def isEmpty: Boolean = false
}

object Test {
  def main(args: Array[String]): Unit = {
    //    val test1 = "Hello" :: "Functional" :: "Scala" :: "World" :: Empty
    //    val test2 = "Hello#" :: "Functional#" :: "Scala#" :: "World#" :: Empty
    //
    //    val result = for (x <- test1) yield x + "#"
    //    result foreach println
    //
    //    val result2 = for (
    //        x <- test1;
    //        y <- test2;
    //        if (y.length > 7)
    //      ) yield (x, y)
    //
    //    result2 foreach println

    //    println(test1.find(_ == "Hello").toLowerCase())
    test
  }

  def test = {

    val result = for (line <- Source.fromFile("myfile.txt").getLines()) yield line
    val result2 = result.filter(!_.trim.equals(""))
      .flatMap(_.split(" ")).toList

    val result3 = NList(result2: _*)
    Console println result3

    //    Console println test1
    //    Console println test2
    //
    //    val filter = (x: String) => x.length() > 4
    //    val map = (x: String) => x + "#!@#"
    //    //
    //        val result1 = test1.filter(filter).map(map).foldRight("")(_ ++ _)
    //        val result2 = test2.filter(filter).map(map).foldRight("")(_ ++ _)
    //    //
    //        Console println result1
    //        Console println result2
    //        Console println "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    //        Console println (result1 == result2)
    //        Console println "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    //
    //    Console println "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    //    Console println test1.foldLeft("")(_ + _)
    //    Console println test2.foldLeft("")(_ + _)
    //    Console println "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    //    Console println test1.foldRight("")(_ + _)
    //    Console println test2.foldRight("")(_ + _)
  }

}