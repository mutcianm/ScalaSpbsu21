package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    val symbols = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    val sb = new StringBuilder
    for (_ <- 1 to length) {
      val randomIndex = util.Random.nextInt(symbols.length)
      sb.append(symbols(randomIndex))
    }
    sb.toString
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = ???

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
  }
}
