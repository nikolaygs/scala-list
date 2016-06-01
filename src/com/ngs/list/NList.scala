package com.ngs.list

import scala.annotation.tailrec

sealed abstract class NList[+T] {
  // basic API methods
  def isEmpty: Boolean
  def head: T
  def tail: NList[T]

  //
  // Combinator library
  //  
  def foldLeft[X](onEmpty: X)(f: (T, => X) => X): X = this match {
    case Empty   => onEmpty
    case x :: xs => f(x, xs.foldLeft(onEmpty)(f))
  }

  def reduceLeft[X](f: (T, => X) => X): X = this match {
    case Empty   => throw new Error("Not supported")
    case x :: xs => f(x, xs.reduceLeft(f))
  }

  def foldRight[X](onEmpty: X)(f: (T, => X) => X): X = this.reverse.foldLeft(onEmpty)(f)

  def filter(f: T => Boolean) = foldLeft(Empty.asInstanceOf[NList[T]]) {
    (x, y) => 
      if (f(x)) x :: y 
      else y
  }

  def map[B](f: T => B) = foldLeft(Empty.asInstanceOf[NList[B]]) {
    (x, y) => f(x) :: y
  }

  def map2[B](f: T => B) = flatMap(x => NList(f(x)))

  def flatMap[B](f: T => NList[B]) = foldLeft(Empty.asInstanceOf[NList[B]]) {
    (x, y) => f(x) ::: y
  }

  def size = foldLeft(0) {
    (x, y) => y + 1
  }

  def contains[B >: T](elem: B) = foldLeft(false) {
    (x, y) => x == elem
  }

  def forall(f: T => Boolean) = foldLeft(false) {
    (x, y) => f(x) && y
  }

  def exists(f: T => Boolean) = foldLeft(false) {
    (x, y) => f(x) || y
  }

  def count(f: T => Boolean) = foldLeft(0) {
    (x, y) => if (f(x)) 1 + y else 0
  }

  def foreach(f: T => Unit) = foldLeft(()) {
    (x, y) => { f(x); y }
  }

  def find(f : T => Boolean): T = reduceLeft[T] {
    (x, y) => if (f(x)) x else x
  }

  def reverse = foldLeft(Empty.asInstanceOf[NList[T]]) {
    def <---[T](result: NList[T], list: NList[T]): NList[T] = list match {
      case Empty   => result
      case x :: xs => <---(x :: result, xs)
    }

    (x, y) => <---(Empty, this)
  }

  def :::[B >: T](that: NList[B]): NList[B] = this match {
    case Empty => that
    case _ => {
      foldLeft(Empty.asInstanceOf[NList[B]]) {
        def ++(list: NList[B], result: NList[B]): NList[B] = list match {
          case Empty   => result
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

  def /:[X](onEmpty: X)(f: (T, => X) => X): X = foldLeft(onEmpty)(f)
  def \:[X](onEmpty: X)(f: (T, => X) => X): X = foldRight(onEmpty)(f)

  //
  // Helper methods
  //
  override def toString = "[" + this.foldLeft("")(_ + ", " + _) + "]"
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
    val test1 = "Hello" :: "Functional" :: "Scala" :: "World" :: Empty
    val test2 = "Hello#" :: "Functional#" :: "Scala#" :: "World#" :: Empty
    test2.contains(new Integer(2))

    val result = for (x <- test1) yield x + "#"
    result foreach println

    val result2 = for (
        x <- test1;
        y <- test2;
        if (y.length > 7)
      ) yield (x, y)

    result2 foreach println

    println(test1.find(_ == "Hello").toLowerCase())
  }

}