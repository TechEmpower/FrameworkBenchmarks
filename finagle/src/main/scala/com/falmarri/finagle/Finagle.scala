package com.falmarri.finagle

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.exp.mysql.{Client, IntValue, MySQL, Row}
import com.twitter.finagle.http.{HttpMuxer, Request, Response}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Future, FuturePool}
import java.net.InetSocketAddress
import java.util.concurrent.Executors
import org.jboss.netty.buffer.ChannelBuffers.wrappedBuffer
import scala.util.Random

object FinagleBenchmark extends App {
  val maxConnections = 256

  val mysql = new Client(ClientBuilder()
    .codec(new MySQL("benchmarkdbuser", "benchmarkdbpass", Some("hello_world")))
    .hosts(new InetSocketAddress(System.getProperty("db.host", "localhost"), 3306))
    .hostConnectionLimit(maxConnections)
    .buildFactory())

  val pool = FuturePool(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 2))

  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def getValue(row: Row, name: String): Any = row(name) match {
    case Some(IntValue(v)) => v
    case _ => throw new Exception("couldn't get value for %s".format(name))
  }

  def rowToMap(row: Row) = {
    Map(
      "id" -> getValue(row, "id"),
      "randomNumber" -> getValue(row, "randomNumber")
    )
  }

  def serialize(result: Any): Array[Byte] =
    mapper.writeValueAsBytes(result)

  def createResponse(req: Request, bytes: Array[Byte]) = {
    val body = wrappedBuffer(bytes)
    val resp = req.response
    resp.setContentTypeJson
    resp.setContent(body)
    resp.contentLength = body.readableBytes
    resp
  }

  HttpMuxer.addRichHandler("/json", new Service[Request, Response] {
    def apply(req: Request): Future[Response] = pool {
      createResponse(req, serialize(Map("message" -> "Hello, World!")))
    }
  })

  HttpMuxer.addRichHandler("/db", new Service[Request, Response] {
    val rand = new Random()
    val sql = "SELECT * FROM world WHERE id = "

    def apply(req: Request): Future[Response] = {
      val n = req.params.getIntOrElse("queries", 1)

      val qs = (0 until n) map { i =>
        mysql.select(sql + rand.nextInt(10000))(rowToMap)
      }

      Future.collect(qs) map { results =>
        createResponse(req, serialize(results.flatten))
      }
    }
  })

  Http.serve(new InetSocketAddress(8080), HttpMuxer)
}
