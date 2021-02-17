package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def isCorrectString(secret: String): Boolean = {
    for (i <- 0 until secret.length) {
      for (j <- 0 until i) {
        if (secret(i) == secret(j)) return false
      }
    }
    true
  }

  def generateNumberString(length: Int): String = {
    var str = Random.nextString(length)
    while (!isCorrectString(str)) {
      str = Random.nextString(length)
    }
    str
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (!isCorrectString(secret)) throw new RepeatingDigitsException()
    if (!isCorrectString(userInput)) throw new RepeatingDigitsException()
    if (userInput.length != secret.length) throw new WrongNumberLengthException(secret.length, userInput.length)
    var bulls = 0
    var cows = 0
    for (i <- 0 until secret.length) {
      if (secret(i) == userInput(i)) bulls += 1
      else for (j <- 0 until secret.length) {
        if (secret(j) == userInput(i)) {
          cows += 1
        }
      }
    }
    if (bulls == secret.length) Correct(numTries)
    else Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Please, input number of digits:")
    val len = readLine().toInt
    val secret = generateNumberString(len)
    var numTries = 0
    var result: GuessResult = Incorrect(-1, -1)
    do {
      print("Please, input your string:")
      numTries += 1
      val userInput = readLine()
      result = validate(secret, userInput, numTries)
      result match {
        case incorrect: Incorrect =>
          println(s"Bulls: ${incorrect.bulls}, cows: ${incorrect.cows}")
        case _ =>
      }
    } while (result.isInstanceOf[Incorrect])
    println(s"Correct! ${result.asInstanceOf[Correct].numTries} tries")
  }
}
