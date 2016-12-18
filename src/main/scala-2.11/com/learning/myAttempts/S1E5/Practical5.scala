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

// Write insertion sort on list of integers
object InsertionSortForInt{
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case head :: tail if head < x => head :: insert(x, tail)
    case _  => x :: xs
  }

  def sort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
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

    def optOrd[T](ord: Ord[T]): Ord[Option[T]] = new Ord[Option[T]] {
      def lt(a: Option[T], b: Option[T]): Boolean = (a, b) match {
        case (Some(x), Some(y)) => ord.lt(x, y)
        case (None, _)          => true
        case (_, None)          => false
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

  Sorter.sort[Int](List(1,2,3,5,2))(Ord.intOrd)
  Sorter.sort[String](List("d","a"))(Ord.strOrd)
  Sorter.sort[Option[Int]](List(Some(1),None, Some(0)))(Ord.optOrd(Ord.intOrd))
  Sorter.sort(List((2, "b"),(1, "c"),(2, "a")))(Ord.tupleOrd(Ord.intOrd, Ord.strOrd))
}

object InsertionSortGenWithImplicit{
  trait Ord[T] {
    def lt(a: T, b: T): Boolean
  }


  object Ord {
    implicit val intOrd: Ord[Int] = new Ord[Int] {
      def lt(a: Int, b: Int): Boolean = a < b
    }

    implicit val strOrd: Ord[String] = new Ord[String] {
      def lt(a: String, b: String): Boolean = a < b
    }

    // There are two ways with which we can define optOrd
    // First way ----> (Commented code to avoid conflict)
    /*implicit def optOrd[T](implicit ord: Ord[T]): Ord[Option[T]] = new Ord[Option[T]] {
      def lt(a: Option[T], b: Option[T]): Boolean = (a, b) match {
        case (Some(x), Some(y)) => ord.lt(x, y)
        case (None, _)          => true
        case (_, None)          => false
      }
    }*/

    //Second Way --->
    implicit def optOrd[T : Ord]: Ord[Option[T]] = new Ord[Option[T]] {
      def lt(a: Option[T], b: Option[T]): Boolean = (a, b) match {
        case (Some(x), Some(y)) => implicitly[Ord[T]].lt(x, y)
        case (None, _)          => true
        case (_, None)          => false
      }
    }

    implicit def tupleOrd[T1, T2](implicit ord1: Ord[T1], ord2: Ord[T2]): Ord[(T1, T2)] = new Ord[(T1, T2)] {
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

    def sort[T](xs: List[T])(implicit ord: Ord[T]): List[T] = xs match {
      case Nil          => Nil
      case head :: tail => insert(head, sort(tail)(ord))(ord)
    }
  }

  Sorter.sort[Int](List(1,2,3,5,2))
  Sorter.sort[String](List("d","a"))
  Sorter.sort[Option[Int]](List(Some(1),None, Some(0)))
  Sorter.sort(List((2, "b"),(1, "c"),(2, "a")))
}
object pimpMyLib {
  class BlingString(string: String) {
    def bling = "*" + string + "*"
  }

  implicit def blingYoString(string: String) = new BlingString(string)

  //Usage :
  val newBlingStr = "Let's get blinged out!".bling
  //Behind the scene this happens --->
  val newBlingStrTrad = blingYoString("Let's get blinged out!").bling
}
