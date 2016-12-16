package com.learning.myAttempts.S1E5

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
