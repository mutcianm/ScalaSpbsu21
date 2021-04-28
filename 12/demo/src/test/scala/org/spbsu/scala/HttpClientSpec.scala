package org.spbsu.scala

import org.scalatest.funsuite.AnyFunSuite
import sttp.client3._
import cats.effect.IO
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import cats.effect.unsafe.implicits.global

import java.io.File

class HttpClientSpec extends AnyFunSuite {
  test("http request to ya.ru returns content") {
    AsyncHttpClientCatsBackend[IO]()
      .flatMap { backend =>
        basicRequest
          .get(uri"https://ya.ru")
          .send(backend)
      }
      .map( resp => println(resp))
      .unsafeRunSync()
  }

  test("download ical from emkn") {
    AsyncHttpClientCatsBackend[IO]()
      .flatMap { backend =>
        basicRequest
          .get(uri"https://emkn.ru/users/446/classes.ics")
          .response(asFile(new File("foo.ical")))
          .send(backend)
      }
      .map( resp => println(resp))
      .unsafeRunSync()
  }

}
