package com.learning.myAttempts.S1E2

import scala.annotation.tailrec

/**
  * Created by Akshay Mhetre on 12/5/2016.
  */
object Practical2 extends App{

  //sum
  def sumR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  //product
  def productR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 1 else fun(a) * productR(fun, a+1,b)


  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, pivot: Int, a: Int, b:Int): Int =
    if(a > b) pivot
    else
      combine(f(a), mapReduce(f, combine, pivot, a+1,b))

  val res =  mapReduce((x:Int) => x*x*x, (a:Int, b:Int)=> a+b, 0, 1,10)
  val res1 =  mapReduce((x:Int) => x*x, (a:Int, b:Int)=> a*b, 0, 1,10)
  println(res)

}
