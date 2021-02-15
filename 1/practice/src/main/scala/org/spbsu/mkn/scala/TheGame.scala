package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random.alphanumeric

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    var secret: List[Char] = List()
    while (secret.length < length) {
      val newSymbol = alphanumeric.take(1)
      if (!secret.contains(newSymbol.toList.head)) {
        secret = newSymbol.toList.head :: secret
      }
    }
    secret.mkString("")
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (secret.length != secret.toSet.size)
      throw new RepeatingDigitsException
    if (secret == userInput)
      return Correct(numTries)
    var bulls, cows = 0
    for (i <- 0 until userInput.length) {
      if (userInput(i) == secret(i)) bulls += 1
      else if (secret.contains(userInput(i))) cows += 1
      }
    Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    println("We are going to play 'Bulls and Cows' game (rules are here: https://en.wikipedia.org/wiki/Bulls_and_Cows)")
    println("Enter the desired string length:")
    val stringLength = readLine().toInt

    val secret = generateNumberString(stringLength)
    var tries = 0

    println(s"I've devised $stringLength character word consisting of 0-9A-Za-z, symbols can't repeat. Try to guess it!")

    var end = false

    while (!end) {
      tries += 1
      println("Enter your string: ")
      val userInput = readLine()
      try {
        validate(secret, userInput, tries) match {
          case Correct(numTries) =>
            println(s"Congratulations! You needed $numTries tries to guess my word!")
            end = true
          case Incorrect(bulls, cows) => println(s"Bulls: $bulls, cows: $cows")
        }

      } catch {
        case e: WrongNumberLengthException => println("Inappropriate length! Be more attentive!")
        case e: RepeatingDigitsException => println("Hmm, there are repeating digits...")

      }
    }
  }
}