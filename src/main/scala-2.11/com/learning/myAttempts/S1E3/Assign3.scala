package com.learning.myAttempts.S1E3

/**
  * Created by Akshay Mhetre on 12/16/2016.
  */
object Assign3 {

}

object assign3 {
  object Answer{
    sealed trait Day
    case object Weekday extends Day
    case object Weekend extends Day

    sealed trait Customer
    case object RegularCustomer extends Customer
    case object RewardsCustomer extends Customer

    case class Category(customer: Customer, day: Day)

    case class Request(customer: Customer, days: List[Day]){
      def categories : List[Category] =
        days.map(d => Category(customer, d))
    }

    case class Hotel(name: String, rating: Int,
                     rateCard: Map[Category, Int]){
      def costOf(request: Request): Int =
        request.categories.map(rateCard).sum
    }

    class HotelService(hotels: Hotel*){
      def bestHotel(request: Request): Hotel =
        hotels.minBy(h => (h.costOf(request), -h.rating))
    }
  }
}


object MemoizeExample {
  def Cache[A,B](f:A=>B) = new Function[A,B] {
    import scala.collection.mutable.Map
    private val cache = Map.empty[A, B]
    override def apply(v:A):B = cache getOrElseUpdate(v,f(v))
  }

  val incrByOneWithCached = Cache( (a:Int) => a + 1 )

  lazy val nth_fib: Int => Int = Cache { (num:Int) =>
    num match {
      case 1 => 0
      case 2 => 1
      case n => nth_fib(n - 1) + nth_fib(n - 2)
    }
  }

  lazy val nth_fib1: Int => Int = Cache {
    case 1 => 0
    case 2 => 1
    case n => nth_fib1(n-1) + nth_fib1(n-2)
  }

  /**
    * Subset sum algorithm - Can you achieve sum n using elements from list?
    **/


  val isSubsetSumAchievable :(((List[Int], Int)) => Boolean) =  Cache {
    case (_, 0) => true         // 0 can always be achieved using empty list
    case (Nil, _) => false      // we can never achieve non-zero if we have empty list
    case (a :: as, x) => isSubsetSumAchievable(as, x - a) || isSubsetSumAchievable(as, x)      // try with/without a.head
  }
  isSubsetSumAchievable(List(1,4,19,7,23,15),12)
  isSubsetSumAchievable(List(1,4,19,7,23,15),19)
  isSubsetSumAchievable(List(1,4,19,7,23,15),25)

}
object MemoizeExample1 {
  def memoize[I, O](f: I => O) = new scala.collection.mutable.HashMap[I, O]() {
    println("Hello")
    override def apply(key: I): O = {
      //val obj = getOrElse(key, s"$key not found")
      //println(obj)
      getOrElseUpdate(key, f(key))
    }
  }

  val multiplyByTwoWithCached = memoize { (num:Int) =>
    num*2
  }
}

object MemoizeExample2 {
  class Memoize1[-T, +R](f: T => R) extends (T => R) {
    import scala.collection.mutable
    private[this] val vals = mutable.Map.empty[T, R]

    override def apply(x: T): R = {
      if (vals.contains(x)) {
        vals(x)
      }
      else {
        val y = f(x)
        vals += ((x, y))
        y
      }
    }
  }

  object Memoize1 {
    def apply[T, R](f: T => R) = new Memoize1(f)
  }

  def strSqLen(s: String) = s.length*s.length

  val strSqLenMemoized = Memoize1(strSqLen)
  val a = strSqLenMemoized("hello Memo")
  val b = strSqLen("hello Memo")
  assert(a == b)
}

// Demo of private vs private[this]

class Foo(foo:Foo){
  private[this] val i = 2
  // Uncomment following line you will see compile time error
  //println(this.i + foo.i)
}

//error: value i is not a member of Foo

class Foo1(foo:Foo1){
  private val i = 2
  println(this.i + foo.i)
}

//defined class Foo

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

    /*implicit def optOrd[T](implicit ord: Ord[T]): Ord[Option[T]] = new Ord[Option[T]] {
      def lt(a: Option[T], b: Option[T]): Boolean = (a, b) match {
        case (Some(x), Some(y)) => ord.lt(x, y)
        case (None, _)          => true
        case (_, None)          => false
      }
    }*/

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

/*    implicit def tupleOrd1[T1, T2 : Ord]: Ord[(T1, T2)] = new Ord[(T1, T2)] {
        def lt(a: (T1, T2), b: (T1, T2)): Boolean = {
          if (implicitly[Ord[T1]].lt(a._1, b._1)) true
          else if (implicitly[Ord[T1]].lt(b._1, a._1)) false
          else implicitly[Ord[T2]].lt(a._2, b._2)
        }
    }*/
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

  def reverseR[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseR(tail) :+ h
    case Nil => Nil
  }

  Sorter.sort[Int](List(1,2,3,5,2))
  Sorter.sort[String](List("d","a"))
  Sorter.sort[Option[Int]](List(Some(1),None, Some(0)))
  Sorter.sort(List((2, "b"),(1, "c"),(2, "a")))
}
