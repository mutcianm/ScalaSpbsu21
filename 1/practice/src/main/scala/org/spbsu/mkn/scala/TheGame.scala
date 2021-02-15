package org.spbsu.mkn.scala

import scala.::
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.io.StdIn.readLine
import scala.util.Random._

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

//  def generateNumberString(length: Int): String = alphanumeric.drop(length).toString()  // если эксклюзивность не важна
  def generateNumberString(length: Int): String = { // если эксклюзивность важна
    if (length < 1) return ""
    val some_string = generateNumberString(length - 1)
    val c = alphanumeric.find(char => !some_string.contains(char)).get
    some_string + c
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.toSet.size != secret.size) throw new RepeatingDigitsException
    if (userInput.size != secret.size) throw new WrongNumberLengthException(secret.size, userInput.size)

    val bulls = userInput.zip(secret).count(pair => pair._1 == pair._2)
    val cows = userInput.toSet.intersect(secret.toSet).size - bulls
    if (bulls == secret.size) return Correct(numTries)
    Incorrect(bulls,cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
  }
}
