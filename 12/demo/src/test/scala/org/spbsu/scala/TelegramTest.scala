package org.spbsu.scala

import org.scalatest.funsuite.AnyFunSuite
import cats.instances.future._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.clients.{FutureSttpClient, ScalajHttpClient}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.clients.FutureSttpClient
import com.bot4s.telegram.future.TelegramBot
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.okhttp.OkHttpFutureBackend

import java.io.FileInputStream
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class TelegramTest extends AnyFunSuite {

  val token: String = new String(new FileInputStream("TG_TOKEN").readAllBytes())

  class MyBot extends TelegramBot
    with Polling
    with Commands[Future] {
    LoggerConfig.factory = PrintLoggerFactory()
    // set log level, e.g. to TRACE
    LoggerConfig.level = LogLevel.TRACE
    implicit val backend: SttpBackend[Future, Nothing] = OkHttpFutureBackend()
    override val client: RequestHandler[Future] = new FutureSttpClient(token)

    onCommand("/ping") {implicit action =>
      reply("pong").void
    }

  }

  test("send TG message") {
    val bot = new MyBot
    val eol = bot.run()
    println("Press [ENTER] to shutdown the bot, it may take a few seconds...")
    scala.io.StdIn.readLine()
    bot.shutdown() // initiate shutdown
    Await.result(eol, Duration.Inf)
  }

}
