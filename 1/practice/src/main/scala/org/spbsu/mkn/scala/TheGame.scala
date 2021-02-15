package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.math.abs
import scala.util.Random

object TheGame {
  private val possibleSymbols = (0 to 9).toList ++ ('A' to 'Z').toList

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    var result = ""
    val generator = new Random()
    var remainingSymbols = possibleSymbols
    for {_ <- 1 to length} {
      val nextIndex = abs(generator.nextInt()) % remainingSymbols.length
      result += remainingSymbols(nextIndex)
      remainingSymbols = remainingSymbols.take(nextIndex) ++ remainingSymbols.drop(nextIndex + 1)
    }
    result
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length) throw new WrongNumberLengthException(secret.length, userInput.length)
    if (secret.length != secret.toSet.size) throw new RepeatingDigitsException
    if (userInput.length != userInput.toSet.size) throw new RepeatingDigitsException
    if (userInput == secret) return Correct(numTries)
    var bulls = 0
    var cows = 0
    for {i <- 0 until secret.length} {
      if (secret(i) == userInput(i)) bulls += 1
      else if (userInput.contains(secret(i))) cows += 1
    }
    Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
  }
}
