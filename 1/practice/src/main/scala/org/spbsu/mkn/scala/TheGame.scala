package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.math.abs
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    var result = ""
    val generator = new Random()
    var possibleSymbols = (0 to 9).toList ++ ('A' to 'Z').toList
    for {_ <- 1 to length} {
      val nextIndex = abs(generator.nextInt()) % possibleSymbols.length
      result += possibleSymbols(nextIndex)
      possibleSymbols = possibleSymbols.take(nextIndex) ++ possibleSymbols.drop(nextIndex + 1)
    }
    result
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = ???

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
  }
}
