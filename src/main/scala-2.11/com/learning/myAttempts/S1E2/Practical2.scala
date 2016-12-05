package com.learning.myAttempts.S1E2

import scala.collection.mutable

/**
  * Created by Akshay Mhetre on 12/5/2016.
  */
object SumOfNumWithShortFun {

  def sumR(fun: Int => Int, a: Int, b:Int):Int = if(a > b) 0 else fun(a) + sumR(fun, a+1,b)


  def sumInts(a: Int, b:Int) =  sumR(a => a, a, b)
  // a => a can also be replaced by identity function from Predef package in Scala
  // def sumInts(a: Int, b:Int) =  sumR(identity, a, b)

  def sumCubes(a: Int, b:Int) =  sumR(a => a*a*a, a, b)
}

object Practical2 {

  def main(args: Array[String]): Unit = {
    println(product(x=>x*x, 1, 3))
    println(sum(x=>x*x*x, 1, 3))
    println(withHello3("akshay"))
    println(withHello3("NAVNEET"))
  }

  //**************** Without Using mapReduce *****************
  //sum
  def sumR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  //product
  def productR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 1 else fun(a) * productR(fun, a+1,b)

  //************** Using Map Reduce ****************

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, pivot: Int, a: Int, b:Int): Int =
    if(a > b) pivot
    else
      combine(f(a), mapReduce(f, combine, pivot, a+1,b))

  def product(f: Int => Int, a: Int, b:Int): Int = mapReduce(f, (x,y) => x*y, 1, a,b)
  def sum(f: Int => Int, a: Int, b:Int): Int = mapReduce(f, (x,y) => x+y, 0, a,b)


  // def vs val
  //1. Check new function creation everytime
  val even: Int => Boolean = _ % 2 == 0 // even eq even return true
  //def even: Int => Boolean = _ % 2 == 0 // even eq even return false

  //2. Check evaluates when define
  val evenPrint: Int => Boolean = {println("Evaluating.."); _ % 2 == 0}
  //def evenPrint: Int => Boolean = {println("Evaluating.."); _ % 2 == 0}

  /*
  * Call By Value and call by name
  *
  * */
  def something() = {
    println("calling something")
    1 // return value
  }
  def callByValue(x: Int) = {
    println("x1=" + x)
    println("x2=" + x)
  }

  //callByValue(something())

  def callByName(x: => Int) = {
    println("x1=" + x)
    println("x2=" + x)
  }

  //callByName(something()) // Output ?


  def foo(a:Int, b:Int, c:Int) = a+b+c

  //Curry version 1 :
  def foo(a:Int)(b:Int)(c:Int) = a+b+c

  //Curry version 2
  var foo = (a:Int) => (b:Int) => (c:Int) => a+b+c

  // With Simplified Brackets :
  //var foo = (a:Int) => ((b:Int) => ((c:Int) => a+b+c))

  //First obvious advantage of currying is iterative parameter passing :

  val lst = List(1,2,3)

  var foo1: (Int) => (Int) => Int = foo(lst(0)) // foo(1)
  var foo2: (Int) => Int = foo1(lst(1)) // foo1(2)
  var foo3: Int = foo2(lst(2)) // foo2(3)

  /*PritnMessage function
  * */
  val myUpperCase: String =>String = str => {
    println(str)
    str toUpperCase
  }

  // Without Currying
  def printWelcomeMsg1(str1: String, str2: String) = println(myUpperCase(str1)+" "+str2)

  val withHello1: (String) => Unit = printWelcomeMsg1("hello", _)  //val withHello1(str:String) = printWelcomeMsg1("hello", str)
  //withHello1("Akshay")

  // Curry Style 1
  def printWelcomeMsg2(str1: String)(str2: String) = println(myUpperCase(str1)+" "+str2)

  val withHello2: (String) => Unit = printWelcomeMsg2("hello")

 // Currying Style 2
  // Notice that we are also making use of Closure feature
  val printWelcomeMsg3: (String) => (String) => Unit = (str:String) => {
      val upper= myUpperCase(str) // Could be Heavy computation
      (d:String) => println(upper+" "+ d)
    }

  val withHello3: (String) => Unit = printWelcomeMsg3("hello")
  // withHello3("Akshay")
  // withHello3("Mhetre") // Will execute myUpperCase exactly once
}

object Combinators{
  def mapInt(lst: List[Int], f : Int => Int): List[Int] = {
    val buf = mutable.Buffer.empty[Int]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    // another way to write loop : Higher level functionality
    /*for(x <- lst){
      buf.append(x)
    }*/
    buf.toList
  }

  val res1 = mapInt(List(1,2,3,4), x => x * x)
  //Could also be written like :
  //val res1 = mapInt((1 to 4).toList, x => x * x)

  def mapGen[A,B](lst: List[A], f : A => B): List[B] = {
    val buf = mutable.Buffer.empty[B]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    /*for(x <- lst){
      buf.append(x)
    }*/
    buf.toList
  }

  val res2 = mapGen(List(1,2,3),{x:Int => x*2}) // Type is required

  def mapCurrGen[A, B](list: List[A])(f: A => B): List[B] = {
    val iterator = list.iterator
    val result = mutable.Buffer.empty[B]

    while (iterator.hasNext) {
      result.append(f(iterator.next()))
    }

    result.toList
  }

  val res3 = mapCurrGen(List(1,2,3))({x => x*2}) // Type is not required
  // Can also be written like this :
  val res33 = mapCurrGen(List(1,2,3))(_*2)
}
