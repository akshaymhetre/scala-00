package com.learning.myAttempts.S1E3

import com.learning.utility.AliceData

/**
  * Created by Akshay Mhetre on 12/9/2016.
  */

class Person(name:String, address: String){
  def this(name:String) = {
    this(name, "")
  }
}

object Person {
  def apply(name: String): Person = new Person(name)
}

object Practical3{
  // Sequence
  val xs = Seq("a", "b", "c")
  //xs: Seq[String]
  xs: Int => String // No Error so xs can have this type also apart from Seq[String]
  xs(2)
  xs.apply(2) // Above code actually calls apply method internally

  //Set
  val ys = Set("a", "b", "c")
  //ys: Set[String]
  ys: String => Boolean
  ys("c")
  ys("aaa")

  //Map
  val hashMap = Map(1 -> "a", 2 -> "b", 3 -> "c")

  hashMap: Map[Int, String]
  //hashMap: Int => String
  //hashMap: scala.collection.MapLike[Int, String]
  hashMap(1)

  val keys = Seq(1, 2, 3)
  keys.map(hashMap)

  val a2z = ('a' to 'z').map(_.toString)
  a2z.filterNot(ys)

  val tup: (Int, Int) = (1,2)
  val tup2 = ("String", 2)
  tup._1

  val birds = List("Golden Eagle", "Gyrfalcon", "American Robin",
    "Mountain BlueBird", "Mountain-Hawk Eagle")
  val groupedByFirstLetter = birds.groupBy(_.charAt(0))

  //Map(M -> List(Mountain BlueBird, Mountain-Hawk Eagle), G -> List(Golden Eagle, Gyrfalcon),
  //   A -> List(American Robin))

  List(1, 2, 3, 1, 4, 2)
    .groupBy(identity)
    .mapValues(_.length)

  (1 to 10)
    .toList
    .take(5)

  List(("Scala",2),("Workshop",3),("python",1), ("Workshop", 2))
    .groupBy(_._1)
    .mapValues(lstTup =>
      lstTup.map(t => t._2).sum
    )

  val p = Person("Akshay")
  p: Person
  //p: String => Int

  val tupleOfTwo = ("GS-0303", "akshay")
  val tuple = (1, "one")
  val tuple1 = 1 -> "one" // Second way to define tuple (only possible for tuple of size two)
  tuple1._1  // to access first element of tuple
  tuple1._2 // to access second element of tuple

}

object AliceDataExercise {

  def findTop10MostOccurringWords = {

    val bookWords = AliceData.bookText
      .split(AliceData.bookRegex)
      .map(_.trim.toLowerCase())

    val stopWords: Set[String] = AliceData.stopWordText
      .split(AliceData.stopWordRegex)
      .map(_.trim.toLowerCase())
      .toSet

    bookWords
      .filterNot(stopWords)
      .groupBy(identity)
      .mapValues(_.length)
      .toSeq
      .sortBy(tup => -tup._2)
      .take(10)
      .foreach(println)
  }
}
