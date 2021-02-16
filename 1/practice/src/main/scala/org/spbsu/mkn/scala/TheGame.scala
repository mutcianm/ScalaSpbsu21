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
      val cows = matchingSymbols - bulls
      Incorrect(bulls, cows)
    }
  }

  var wordIsGuessed = false
  var numberOfTries = 0

  def tryToGuess(secret: String): GuessResult = {
    print("Try to guess: ")
    val userInput = readLine()
    var result: GuessResult = null

    try {
      result = validate(secret, userInput)
      result match {
        case _: Correct =>
          println("Right! The number of your tries: " + numberOfTries)
          wordIsGuessed = true
        case result: Incorrect =>
          println("Not right. Bulls: " + result.bulls + ", cows: " + result.cows)
      }
    } catch {
      case _: WrongNumberLengthException => println("Wrong length of your input. Try again.")
      case _: RepeatingDigitsException => println("Repeating digits in your input. Try again.")
    }
    result
  }

  def main(args: Array[String]): Unit = {
    print("Welcome to the game! Enter the length of the word you will guess: ")
    val length = readLine().toInt
    val secret = generateNumberString(length)

    while (!wordIsGuessed) {
      numberOfTries += 1
      tryToGuess(secret)
    }
  }
}


