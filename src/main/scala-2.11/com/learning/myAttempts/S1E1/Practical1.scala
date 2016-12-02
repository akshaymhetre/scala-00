package com.learning.myAttempts.S1E1

/**
  * Created by Akshay Mhetre on 12/2/2016.
  */
object Practical1 extends App{
  println("Practicals done for session 1")
  println(SumOfNum.sumL(1,10))
  println(SumOfNum.sumR(1,10))
  println(SumOfNum.sumTR(1,10))
  println(SumOfNumWithFun.sumCubes(1,10))
  println(SumOfNumWithFun.sumFact(1,10))
}


object SumOfNum {
  //Loop : Traditional way
  def sumL(a: Int, b:Int) = {
    if(a > b) 0
    else {
      var sum = 0
      //var _a = a
      for(i <- a to b){
        //while(_a <= b) {
        sum += i // sum += _a
        //_a += 1
      }
      sum
    }
  }

  //Recursive way
  def sumR(a: Int, b:Int):Int = if(a > b) 0 else a + sumR(a+1,b)

  def sumTR(a: Int, b:Int) = {
    //Tail Recursive way
    def _sumTR(a: Int, b:Int, acc: Int):Int = {
      if(a > b) acc
      else _sumTR(a+1, b, acc+a)
    }
    _sumTR(a, b, 0)
  }


  // Traditional repetitive way
  def cube(a:Int) = a*a*a
  def sumCubes(a: Int, b:Int):Int =  if(a > b) 0 else cube(a) + sumCubes(a+1,b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def sumFact(a: Int, b:Int):Int =  if(a > b) 0 else fact(a) + sumFact(a+1,b)

}

// Higher Order Functions way
object SumOfNumWithFun {

  def sumR(fun: Int => Int, a: Int, b:Int):Int = if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  def cube(a:Int) = a*a*a
  def sumCubes(a: Int, b:Int) =  sumR(cube, a, b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def sumFact(a: Int, b:Int):Int =  sumR(fact, a, b)

}