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
    val rnd = Random.alphanumeric
    var genStr = ""
    var cnt = -1
    while (genStr.length < length) {
      cnt += 1
      val symb = rnd(cnt)
      if (genStr.indexOf(symb) == -1)
        genStr += symb
    }
    genStr
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (secret.toSet.size != secret.length || userInput.toSet.size != userInput.length)
      throw new RepeatingDigitsException

    if (secret == userInput) Correct(numTries)
    else {
      var cntCow = 0
      var cntBull = 0
      for (i <- 0 until secret.length) {
        val ind = userInput.indexOf(secret(i))
        ind match {
          case x if x == i => cntBull += 1
          case x if x != -1 => cntCow += 1
          case _ =>
        }
      }
      Incorrect(cntBull, cntCow)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Enter length of secret string")
    val length = readLine().toInt
    val secret = TheGame.generateNumberString(length)
    var attempts = 0
    var gameGoOn = true
    while (gameGoOn) {
      println("Enter your query")
      val query = readLine()
      attempts += 1
      TheGame.validate(secret, query, attempts) match {
        case Correct(x) => println(s"You guess string in $x queries"); gameGoOn = false
        case Incorrect(b, c) => println(s"You have $b bulls and $c cows")
      }
    }
  }
}
