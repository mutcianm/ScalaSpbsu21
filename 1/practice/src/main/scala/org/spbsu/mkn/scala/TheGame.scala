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
    Random.shuffle(symbols).toString.take(length)
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    else if (secret.length != userInput.toSet.size)
      throw new RepeatingDigitsException
    else if (secret == userInput)
      Correct(numTries)
    else {
      val matchingSymbols = userInput.toSet.intersect(secret.toSet).size
      val bulls = userInput.zipWithIndex.count(i => secret.zipWithIndex.contains(i))
      Incorrect(bulls, matchingSymbols - bulls)
    }
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
  }
}
