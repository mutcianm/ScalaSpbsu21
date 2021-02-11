package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int, chars: Set[Char] = (('A' to 'Z')++ ('0' to '9')).toSet): String = {
    val char = chars.iterator.drop(Random.nextInt(chars.size)).next()
    length match {
      case 0 => ""
      case _ => char.toString + generateNumberString(length - 1, chars.diff(Set(char)))
    }
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length) throw new WrongNumberLengthException(secret.length, userInput.length)

    if (secret == userInput) {
      if (userInput.toSet.size != userInput.length) throw new RepeatingDigitsException
      return Correct(numTries)
    }

    val bulls = secret.zip(userInput).map {case (a, b) => a == b}.count(x => x)
    val cows = secret.toSet.intersect(userInput.toSet).size
    Incorrect(bulls, cows - bulls)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
  }
}
