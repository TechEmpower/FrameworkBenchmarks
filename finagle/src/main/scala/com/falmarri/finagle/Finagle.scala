package com.falmarri.finagle

import scala.util.Random
import scala.collection.immutable.StringOps
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.util.concurrent.Executors
import com.twitter.finagle.Service
import com.twitter.finagle.exp.Mysql
import com.twitter.finagle.exp.mysql._
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import org.jboss.netty.handler.codec.http.HttpVersion.HTTP_1_1
import org.jboss.netty.buffer.ChannelBuffers.wrappedBuffer
import com.twitter.util.{Future, FuturePool}
import java.net.InetSocketAddress
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.http.{Http,HttpMuxer}

object FinagleBenchmark extends App {
  val maxConnections = 256

  //val mysql = new Client(ClientBuilder()
  //  .codec(new MySQL("benchmarkdbuser", "benchmarkdbpass", Some("hello_world")))
  //  .hosts(new InetSocketAddress(System.getProperty("db.host", "localhost"), 3306))
  //  .hostConnectionLimit(maxConnections)
  //  .buildFactory())

  val username = "benchmarkdbuser"
  val password = "benchmarkdbpass"
  val db = "hello_world"
  val host = System.getProperty("db.host", "localhost")

  val mysql = Mysql
      .withCredentials(username, password)
      .withDatabase(db)
      .newRichClient(host + ":3306")

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

  def createResponse(req: HttpRequest, bytes: Array[Byte]) = {
    val body = wrappedBuffer(bytes)
    val resp = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
    //resp.setContentTypeJson
    resp.setContent(body)
    //resp.contentLength = body.readableBytes
    resp
  }

  val muxService = new HttpMuxer()
    .withHandler("/json", new Service[HttpRequest, HttpResponse] {
      def apply(req: HttpRequest): Future[HttpResponse] = pool {
        createResponse(req, serialize(Map("message" -> "Hello, World!")))
      }
    })
    .withHandler("/db", new Service[HttpRequest, HttpResponse] {
      val rand = new Random()
      val sql = "SELECT * FROM world WHERE id = "

      def apply(req: HttpRequest): Future[HttpResponse] = {
        //val n = req.params.getIntOrElse("queries", 1)
        val decoder = new QueryStringDecoder(req.getUri())
        val n = {
          val queries = decoder.getParameters().get("queries")
          if(queries == null) {
            1
          }
          else {
            queries.get(0).toInt
          }
        }

        val qs = (0 until n) map { i =>
          mysql.select(sql + rand.nextInt(10000))(rowToMap)
        }

        Future.collect(qs) map { results =>
          createResponse(req, serialize(results.flatten))
        }
      }
    })

  //Http.serve(new InetSocketAddress(8080), HttpMuxer)
  val server: Server = ServerBuilder()
    .codec(Http())
    .bindTo(new InetSocketAddress(8080))
    .name("HttpServer")
    .build(muxService)
}
