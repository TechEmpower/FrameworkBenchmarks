package com.falmarri.finagle

import com.twitter.finagle.http.{ Request, Response, RichHttp, Http }
import com.twitter.finagle.builder.{ Server, ServerBuilder }
import java.net.InetSocketAddress
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.Service
import com.twitter.util.Future
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.handler.codec.http.DefaultHttpResponse
import org.jboss.netty.handler.codec.http.HttpResponseStatus
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scala.language.experimental.macros
import scala.reflect.macros.Context
import java.io.StringWriter
import org.jboss.netty.util.CharsetUtil.UTF_8
import scala.slick.driver.MySQLDriver.simple._
import scala.util.Random
import org.apache.commons.dbcp.BasicDataSource
import javax.sql.DataSource
import com.twitter.util.FuturePool
import java.util.concurrent.Executors

case class World(id: Int, randomNumber: Int)

object Worlds extends Table[World]("World") {
  def id = column[Int]("id", O.PrimaryKey)
  def randomNumber = column[Int]("randomNumber")
  def * = id ~ randomNumber <> (World, World.unapply _)
  
}


object FinagleBenchmark extends App {

//  def serialize[T](value: T): String = macro serializeImpl[T]
//
//  def serializeImpl[T](c: Context)(value: c.Expr[T]): c.Expr[String] = {
//    import java.io.StringWriter
//    import c.universe._
//    c.Expr[String](reify {
//          val writer = new StringWriter()
//          mapper.writeValue(writer, value.splice)
//          writer.toString()
//    }.tree)
//  }
  val dataSource: DataSource = {
    val ds = new BasicDataSource
    ds.setDriverClassName("com.mysql.jdbc.Driver")
    ds.setUsername("benchmarkdbuser")
    ds.setPassword("benchmarkdbpass")
    ds.setMaxActive(20);
    ds.setMaxIdle(10);
    ds.setInitialSize(20);
    //ds.setValidationQuery("SELECT 1 FROM INFORMATION_SCHEMA.SYSTEM_USERS")
    //new java.io.File("target").mkdirs // ensure that folder for database exists
    ds.setUrl("jdbc:mysql://" + System.getProperty("db.host", "localhost") + ":3306/hello_world")
    ds
  }
  
  val database = Database.forDataSource(dataSource)
  
  def serialize(value: Any) = {
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString()
  }

  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  val json = new Service[Request, Response] {
    def apply(req: Request): Future[Response] ={
      val resp = Response()
      resp.setContent(copiedBuffer(serialize(Map("message" -> "Hello, World!")), UTF_8))
      resp.setContentTypeJson
      Future.value(resp)
    }

  }
  
  val diskIoFuturePool = FuturePool(Executors.newFixedThreadPool(8))
 

  val db = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val n = req.params.getIntOrElse("queries", 1)
      val resp = Response()
      database withSession {implicit session: Session =>
        val rand = new Random()
        val q = for (i <- 0 until n) yield (for {w <- Worlds if w.randomNumber === rand.nextInt(10000)} yield w)

        
        //val q = Query(Worlds).where(_.id inSet( for (i <- 0 to n) yield rand.nextInt(10000)))
        resp.setContent(copiedBuffer(serialize(if (n == 1) q.head else q), UTF_8))
        resp.setContentTypeJson
        Future.value(resp)
      }
    }
  }
  
  val poolingdb = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val n = req.params.getIntOrElse("queries", 1)
      val query = {
	      val resp = Response()
	      database withSession {implicit session: Session =>
	        val rand = new Random()
	        val q = Query(Worlds).where(_.id inSet( for (i <- 0 to n) yield rand.nextInt(10000)))
	        resp.setContent(copiedBuffer(serialize(if (n == 1) q.first else q.list), UTF_8))
	        resp.setContentTypeJson
	        resp
	      	}
      	}
      diskIoFuturePool(query)
    }
  }
  

  val service =
    RoutingService byPath {

      case "/json" => json
      case "/db" => db
      case "/pooling" => poolingdb

    }

  val server: Server = ServerBuilder()
    .codec(RichHttp[Request](Http()))
    .bindTo(new InetSocketAddress(8080))
    .name("finagle")
    .build(service)

}
