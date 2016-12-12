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

  case class AppleBox(item: Apple, quantity: Int)
  case class OrangeBox(item: Orange, quantity: Int)

  case class BoxG[+A <: Fruit](item : A, quantity : Int){
    def label: String = item.name
  }

  def unWrappedFruitBox(box: BoxG[Fruit])
  = s"This box contains fruits : ${box.label} with quantity ${box.quantity} in dozens"

  val apple = new Apple()
  val appleBox: BoxG[Fruit] = BoxG[Apple](apple,3)
  unWrappedFruitBox(appleBox)

  val listOfVariables = List(1, 2, 2.7, "One", "two", 'symbolExample)

  for{
    variable <- listOfVariables
  }{
    val str = variable match {
      case 1 => "int 1"
      case i:Int => "other int: "+i
      case d:Double => s"double: $d"
      case "one" => "String one"
      case _: String => "other string"
      case _ => "Other type of variable"
    }
    println(str)
  } // For loop end

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
