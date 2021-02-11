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
    val l = (0 until 36).map(i => {
      if (i < 10) {
        (i + '0').toChar
      } else {
        (i - 10 + 'A'.toInt).toChar
      }
    }).toList

    val perm = Random.shuffle((0 until 36).toList).take(length)
    var str = ""
    perm.foreach {i => str = str + l(i)}
    str
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    val lenS = secret.length
    val lenUI = userInput.length

    if (lenS != lenUI) {
      throw new WrongNumberLengthException(lenS, lenUI)
    }

    val setUI = userInput.toSeq.toSet
    val setS = secret.toSeq.toSet

    if (setUI.size != lenUI) {
      throw new RepeatingDigitsException
    }

    var bulls = setUI.intersect(setS).size
    var cows = 0

    for (i <- 0 until lenS) {
      if (secret(i) == userInput(i)) {
        cows += 1
      }
    }
    bulls -= cows

    if (cows == lenS) {
      Correct(numTries)
    } else {
      Incorrect(cows, bulls)
    }
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    print("Enter length: ")
    val length = readLine().toInt

    val secret = generateNumberString(length)
    var guessed = false
    var tries = 0

    while (!guessed) {
      val userInput = readLine()
      tries += 1

      try {
        val res = validate(secret, userInput, tries)
        res match {
          case _: Correct =>
            println(s"Correct! You take ${tries} tries")
            guessed = true
          case res: Incorrect =>
            println(s"Bulls ${res.bulls}; cows ${res.cows}")
          case _ =>
        }
      } catch {
        case e:RepeatingDigitsException => println("Repeating digits are not allowed!")
        case e:WrongNumberLengthException => println(s"Wrong length!")
      }
    }
  }
}
