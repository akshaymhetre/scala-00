package com.learning.myAttempts.S1E4

/**
  * Created by Akshay Mhetre on 12/12/2016.
  */
object assign4 {
  //http://blog.thedigitalcatonline.com/blog/2015/04/07/99-scala-problems-08-eliminate-consecutive-duplicates/#.WE6mr7J97IU
  def compress[A](l: List[A]):List[A] = l match {
    case Nil => Nil
    case h::List() => List(h)
    case h::tail if (h == tail.head) => compress(tail)
    case h::tail => h::compress(tail)
  }

  def compress1[A](l: List[A]):List[A] = l.foldLeft(List[A]()) {
    case (List(), e) => List(e)
    case (ls, e) if (ls.last == e) => ls
    case (ls, e) => ls:::List(e)
  }

  def compress2[A](l: List[A]):List[A] = l.foldLeft(List[A]()) {
    case (ls, e) if (ls.isEmpty || ls.last != e) => ls:::List(e)
    case (ls, e) => ls
  }

  def compress3[A](l: List[A]):List[A] = l.foldRight(List[A]()) {
    case (e, ls) if (ls.isEmpty || ls.head != e) => e::ls
    case (e, ls) => ls
  }

  // Assignment
  def reverse[A](l:List[A]):List[A] = {
    def _reverse[A](r:List[A], l:List[A]):List[A] = l match {
      case Nil => r
      case head::tail => _reverse(head::r, tail)
    }
    _reverse(List(), l)
  }

  def reverseF[A](l:List[A]):List[A] = l.foldLeft(List[A]()){
    (acc, lstElem) =>  lstElem :: acc
  }

  def reverseR[A](l: List[A]): List[A] = l match {
    case h :: tail => reverseR(tail) ::: List(h)
    case Nil => Nil
  }




}
