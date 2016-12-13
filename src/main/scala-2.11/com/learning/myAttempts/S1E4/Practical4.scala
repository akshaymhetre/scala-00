package com.learning.myAttempts.S1E4


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

object FruitExamplePractical {
  import FruitExample._

  // Design apple box
  case class AppleBox(item: Apple, quantity: Int)
  case class OrangeBox(item: Orange, quantity: Int)

  case class GBox[F <: Fruit](item: F, quantity: Int){
    def label = item.name
  }

  def unWrappedFruitBox1(box: GBox[Fruit]) = s"This box contains fruits : ${box.label} with quantity ${box.quantity} in dozens"


  //var box11: Box[Orange] = GBox[Orange](new Orange, 2)  // Compile time error

  //Final design
  case class Box[+F <: Fruit](item: F, quantity: Int){
    def label = item.name
  }

  // 3. Write unwrapped box mehtod
  def unWrappedFruitBox(box: Box[Fruit]) = s"This box contains fruits : ${box.label} with quantity ${box.quantity} in dozens"

  // call this method with orange box
  var box1: Box[Orange] = Box[Orange](new Orange, 2)

  unWrappedFruitBox(box1)

}

//Pattern Matching examples
object PMForCollections{
  val langs = Seq(
    ("Scala","Odersky"),
    ("Scala", "Akshay"),
    ("Clojure", "Rich"),
    ("Lisp", "Jhon")
  )

  // PM on tuples
  for(book <- langs){
    val str = book match {
      case ("Scala", author) if author.length < 5 => "Found Scala book by Odersky"
      case ("Scala", _) => "Found other Scala books"
      case (lang, author) => s"Found other languages books $lang:$author"
    }
    println(str)
  }

  // If odersky book : "Found Scala book by Odersky"
  // "Found other Scala books"
  //"Found other languages books $lang:$author"

  // Case classes : Deep matching
  case class Address(street: String, city: String, country: String)
  case class Person(name: String, age: Int, address: Address)

  val alice = Person("Alice", 25,
    Address("1 Scala Lane", "Chicago", "USA"))
  val charlie = Person("Charlie", 32,
    Address("3 Python Ct.", "Boston", "USA"))
  val ali = Person("Ali", 25,
    Address("1 Lisp Lane", "Chicago", "USA"))
  val bob = Person("Bob", 29,
    Address("2 Java Ave.", "Miami", "USA"))

  val persons = Seq(alice, charlie,  ali, bob)

  // Say 'Hi $name' to people from Chicago
  // and for others just say 'Goodbye $name'

  for(person <- persons){
    person match {
      case Person(name, _, Address(_, "Chicago", _)) =>
        s"Hi $name"
      case Person(name, _, _) =>
        s"GoodBye $name"
    }
  }

  def printHeadOfList[T](lst : List[T]) = lst match {
    case h1 :: _ => println(h1)
    case Nil => println("List is empty")
  }

  def getHeadOfList[T](lst : List[T]):Option[T] = lst match {
    case h1 :: _ => Some(h1)
    case Nil => None
  }

  def getSecondHeadOfList[T](lst : List[T]):Option[T] = lst match {
    case _ :: h2 :: _ => Some(h2)
    case _ => None
  }

  // find out length of the list (Recursion)
  def lengthOfList[T](lst : List[T]):Int = lst match {
    case head :: tail => 1+ lengthOfList(tail)
    case Nil => 0
  }

  // find out length of the list (Tail-Recursion)
  def lengthOfListTR[T](lst : List[T]) = {
    def _lengthOfListTR[T](lst : List[T], acc: Int):Int = lst match {
      case head :: tail => _lengthOfListTR(tail, acc+1)
      case Nil => acc
    }
    _lengthOfListTR(lst, 0)
  }

  //foldLeft way
  def lengthOfListFL[T](lst : List[T]):Int = lst.foldLeft(0)((acc,ele) => acc+1)

  //List creation in a literal way
  //val lst = List(1,2,3,4)
  //val lst2 = 1 :: 2 :: 3 :: 4 :: Nil
  //((Nil.::(4)).::(3))

  printHeadOfList(List(1,2,3,4))

  // Dealing with option
  val name1: Option[String] = Some("akshay")
  val name2: Option[String] = None

  val name: Option[String] = name2
  name.getOrElse("")

  val nameVal = name match {
    case Some(actualName) => s"$actualName"
    case  _ => "No value"
  }

  name.map(_.trim).filter(_.length > 0).getOrElse("")
}

// Extractor usage
// How to use unapply
object ExtractorExample{

  object Email{
    def unapply(email: String): Option[(String, String)] =
      email.split("@") match {
        case Array(name, domain) => Some(name, domain)
        case _ => None
      }
  }

  val email = "akshay@gslab.com"
  val Email(name, domain) = email
  // which will give user = akshay and domain = gslab.com

  email match {
    case Email(name, domain) => s"My $name & from $domain"
    case _ => s"Invalid email"
  }

}

// Renaming above class with @@
// To achieve readability (seriously ?)
object EmailExample {

  object @@ {
    def unapply(email: String): Option[(String, String)] = email.split("@") match {
      case Array(user, domain) => Some(user, domain)
      case _ => None
    }
  }

  val user @@ domain = "akshay@gslab.com"
  // which will give user = akshay and domain = gslab.com

  def trans(email: String): String = email match {
    case user @@  domain => s"$user--$domain"
    case _ => "error"
  }

}
