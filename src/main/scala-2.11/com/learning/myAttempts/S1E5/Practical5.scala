package com.learning.myAttempts.S1E5

import com.learning.myAttempts.S1E5.Ordering.Ord

/**
  * Created by Akshay Mhetre on 12/16/2016.
  */
object Practical5 extends App {
  def sort(xs: List[Int]):List[Int] = xs match {
    case head :: tail => insert(head, sort(tail))
    case Nil => xs
  }

  def insert(elem:Int, xs : List[Int]):List[Int] = xs match {
    case Nil => List(elem)
    case head :: tail if head < elem => head :: insert(elem, tail)
    case _ => elem :: xs
  }
}

object Ordering {

}

// Write insertion sort on list of integers
object InsertionSortForInt{
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil                             => List(x)
    case head :: tail if head < x => head :: insert(x, tail)
    case head :: tail                    => x :: xs
  }

  def sort(xs: List[Int]): List[Int] = xs match {
    case Nil          => Nil
    case head :: tail => insert(head, sort(tail))
  }

  sort(List(2,5,1,3))
}

// Make it generic
object InsertionSortGen{
  trait Ord[T] {
    def lt(a: T, b: T): Boolean
  }

  object Ord {
    val intOrd: Ord[Int] = new Ord[Int] {
      def lt(a: Int, b: Int): Boolean = a < b
    }

    val strOrd: Ord[String] = new Ord[String] {
      def lt(a: String, b: String): Boolean = a < b
    }

  }

  object Sorter {
    private def insert[T](x: T, xs: List[T])(ord: Ord[T]): List[T] = xs match {
      case Nil                             => List(x)
      case head :: tail if ord.lt(head, x) => head :: insert(x, tail)(ord)
      case head :: tail                    => x :: xs
    }

    def sort[T](xs: List[T])(ord: Ord[T]): List[T] = xs match {
      case Nil          => Nil
      case head :: tail => insert(head, sort(tail)(ord))(ord)
    }
  }

  Sorter.sort[Int](List(1,2,3,5,2))(Ord.intOrd)

}

