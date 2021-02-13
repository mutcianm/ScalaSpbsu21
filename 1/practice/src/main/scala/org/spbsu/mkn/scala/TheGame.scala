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
    var list: List[Int] = ((48 to 57).concat(97 to 122).toList)
    list = scala.util.Random.shuffle(list)
    val str: Array[Char] = new Array[Char](length)
    for (i <- (0 to (length - 1))) str(i) = (list(i)).toChar
    return new String(str)
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length) {
      throw new WrongNumberLengthException(secret.length, userInput.length)
    }
    for (i <- (1 until secret.length)) {
      for (j <- (0 until i)) {
        if (userInput(i) == userInput(j)) {
          throw new RepeatingDigitsException()
        }
      }
    }
    if (secret == userInput) {
      return Correct(numTries)
    }
    var bulls: Int = 0
    var cows: Int = 0
    for (i <- (0 until secret.length)) {
      for (j <- (0 until secret.length)) {
        if (userInput(i) == secret(j)) {
          if (i == j) {
            bulls += 1
          } else {
            cows += 1
          }
        }
      }
    }
    return Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
    var i: Int = 0
    val str: String = generateNumberString(6)
    while (true) {
      i += 1
      val line: String = readLine()
      try {
        val res: GuessResult = validate(str, line, i)
        res match {
          case Correct(a) => println(s"YouWon Tries: $a")
            return 0
          case Incorrect(bulls, cows) => println(s"Incorerect bulls: $bulls cows: $cows")
        }
      } catch {
        case e: WrongNumberLengthException => println(s"неправильное количество символов, надо " + str.length)
        case e: RepeatingDigitsException => println("Repeating numbers/chars")
      }
    }
  }
}
