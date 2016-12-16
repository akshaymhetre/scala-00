package com.learning.myAttempts.S1E5



/**
  * Created by Akshay Mhetre on 12/16/2016.
  */
object Practical5 {
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
object InsertionSortForInt extends App{
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
object InsertionSortGen1 extends App{
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

    def optOrd[T](ord: Ord[T]):Ord[Option[T]] = new Ord[Option[T]] {
      def lt(a: Option[T], b: Option[T]): Boolean = (a, b) match {
        case (Some(a), Some(b)) => ord.lt(a,b)
        case (None, _) => true
        case _ => false
      }
    }

    def tupleOrd[T1, T2](ord1: Ord[T1], ord2: Ord[T2]): Ord[(T1, T2)] = new Ord[(T1, T2)] {
      def lt(a: (T1, T2), b: (T1, T2)): Boolean = {
        if (ord1.lt(a._1, b._1)) true
        else if (ord1.lt(b._1, a._1)) false
        else ord2.lt(a._2, b._2)
      }
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

  Sorter.sort[Int](List(1,2,3,5,2))(Ord.intOrd).foreach(println)

  Sorter.
    sort[Option[Int]](List(Some(1),None, Some(0)))(Ord.optOrd(Ord.intOrd)).foreach(println)

  val lstOfTup = List((5,"akshay"),(2,"rahul"))

  Sorter.sort[(Int, String)](lstOfTup)(Ord.tupleOrd(Ord.intOrd,Ord.strOrd))

}

