package com.learning.utility

/**
  * Created by Akshay Mhetre on 12/12/2016.
  */
object FruitExample{
  trait Fruit {
    def name: String
    def printName = println(s"Fruit is $name")
  }

  class Orange extends Fruit { def name = "Orange" }

  class Apple extends Fruit { def name = "Apple" }
}


object HelperForS1E4 {

}

object PMForCollections{
  val langs = Seq(
    ("Scala","Odersky"),
    ("Scala", "Akshay"),
    ("Clojure", "Rich"),
    ("Lisp", "Jhon")
  )

  // If odersky book : "Found Scala book by Odersky"
  // "Found other Scala books"
  //"Found other languages books $lang:$author"

  // Case classes : Deep matching
  case class Address(street: String, city: String, country: String)
  case class Person(name: String, age: Int, address: Address)

  val alice = Person("Alice", 25, Address("1 Scala Lane", "Chicago", "USA"))
  val charlie = Person("Charlie", 32, Address("3 Python Ct.", "Boston", "USA"))
  val ali = Person("Ali", 25, Address("1 Lisp Lane", "Chicago", "USA"))
  val bob = Person("Bob", 29, Address("2 Java Ave.", "Miami", "USA"))

  val persons = Seq(alice, charlie,  ali, bob)

  // Say 'Hi $name' to people from Chicago and for others just say 'Goodbye $name'


}
