package com.learning.myAttempts.S1E1

import scala.annotation.tailrec

/**
  * Created by Akshay Mhetre on 12/4/2016.
  */
object Assign1 {
  //sum
  def sumR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  //product
  def productR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 1 else fun(a) * productR(fun, a+1,b)

  def id(a:Int) = a
  def prodNums(a: Int, b:Int) =  productR(id, a, b)

  def cube(a:Int) = a*a*a
  def prodCubes(a: Int, b:Int) =  productR(cube, a, b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def prodFact(a: Int, b:Int):Int =  productR(fact, a, b)

  //Tail Recursive factorial
  def factTR(a:Int):Int = {
    @tailrec
    def _factTR(a:Int, acc: Int):Int = {
      if(a < 2) acc else _factTR(a-1, acc*a)
    }
    _factTR(a, 1)
  }

  //Fibonacci Rec
  def fibR(a: Int):Int = if(a == 1) 0 else if(a == 2) 1 else fibR(a-1)+fibR(a-2)

  //Fibonacci tail recursive
  def fibTR(i: Int) = {
    @tailrec
    def _fibTR(i: Int, a : Int, b: Int):Int = {
      if(i == 1) a else if(i == 2) b else _fibTR(i-1, b, a+b)
    }
    _fibTR(i, 0, 1)
  }

}
