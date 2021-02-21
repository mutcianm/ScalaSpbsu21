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
    val initialString = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).mkString("")
    val shuffledString = Random.shuffle(initialString).mkString("")
    shuffledString.substring(26 + 26 + 10 - length)
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length) {
      throw new WrongNumberLengthException(secret.length, userInput.length)
    } else if (secret.length != secret.toSet.size || userInput.length != userInput.toSet.size) {
      throw new RepeatingDigitsException()
    } else if (secret == userInput) {
      Correct(numTries)
    } else {
      val cowsCount = secret.toSet.intersect(userInput.toSet).size
      val bullsCount = (0 until secret.length).toArray.count(index => secret(index) == userInput(index))
      Incorrect(bullsCount, cowsCount - bullsCount)
    }
  }

  def main(args: Array[String]): Unit = {
    print("Hi!\nEnter your name: ")
    val name = readLine()
    println(s"Hello, $name!\nLet's play. Enter secret length:")
    val secret = generateNumberString(readLine().toInt)
    println("Possible symbols: " ++ (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).mkString(""))
    println("Leave your guess:")
    var numTries = 0
    while (true) {
      try {
        val guessResult = validate(secret, readLine(), numTries)
        numTries += 1
        guessResult match {
          case Incorrect(bulls, cows) =>
            println("You got more information:")
            println("bulls: " ++ bulls.toString ++ " and cows: " ++ cows.toString)
            println("Try again!")
          case _ =>
            println("Congratulations! The secret was: " ++ secret ++ "\nPLay again!")
            println("It took " ++ numTries.toString ++ " only steps!")
            return
        }
      } catch {
        case e: WrongNumberLengthException => println("")
        case e: RepeatingDigitsException => println("")
      }
    }
  }
}
