package com.learning.myAttempts.S1E5

/**
  * Created by Akshay Mhetre on 12/18/2016.
  */
object Assign5 {
  val fstTree = MyTree.createBSTFromList(List(2,1,6,4,7,3,5))
  println(fstTree)
  MyTree.printTreeInfix(fstTree)
  println()
  println(MyTree.leafNodes(fstTree))
}

object MyTree{
  sealed trait Tree[+A]
  case object Leaf extends Tree[Nothing]
  case class Node[A](left: Tree[A], data: A, right: Tree[A]) extends Tree[A]

  //Companion object of Tree
  case object Tree{
    def Empty[A]:Tree[A] = Leaf  //Kind of static method
  }

  def create() = {
    Node(
      Node(Leaf, 3, Leaf),
      5,
      Node(Leaf, 7, Leaf)
    )
  }

  def createBSTFromList(lst: List[Int]):Tree[Int] = {
    lst.foldLeft(Tree.Empty[Int])((root, num) => insertIntoList(root, num))
  }

  def insertIntoList(root: Tree[Int], num: Int): Tree[Int] = {
    root match {
      case Leaf => Node(Leaf, num, Leaf)
      case Node(left, data, right) if (num < data) => Node(insertIntoList(left, num), data, right)
      case Node(left, data, right) => Node(left, data, insertIntoList(right, num))
    }
  }

  def printTreeInfix(root: Tree[Int]):Unit = {
    root match {
      case Node(left, data, right) => {
        printTreeInfix(left)
        print(data)
        printTreeInfix(right)
      }
      case Leaf => print("")
    }
  }

  def leafNodes(root: Tree[Int]): Int = {
    root match {
      case Leaf => 0
      case Node(Leaf, _, Leaf) => 1
      case Node(left, _, right) => leafNodes(left) + leafNodes(right)
    }
  }
}

