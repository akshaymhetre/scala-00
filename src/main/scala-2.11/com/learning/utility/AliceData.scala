package com.learning.utility

/**
  * Created by Akshay Mhetre on 10/20/2016.
  */
import scala.io.Source

object AliceData {
  val bookText = Source.fromInputStream(getClass.getResourceAsStream("/aliceInWonderland.txt")).mkString
  val stopWordText = Source.fromInputStream(getClass.getResourceAsStream("/stopWords.txt")).mkString

  val bookRegex = """[\s|:|.|,|"]+"""
  val stopWordRegex = "\\s+"
}

