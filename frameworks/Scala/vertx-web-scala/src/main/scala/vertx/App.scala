package vertx

import java.io.{ByteArrayOutputStream, File, IOException}
import java.nio.file.Files
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.scalalogging.Logger
import io.vertx.core.buffer.Buffer
import io.vertx.core.http.HttpHeaders
import io.vertx.core.json.{JsonArray, JsonObject}
import io.vertx.core.{AsyncResult, Handler, VertxOptions => JVertxOptions}
import io.vertx.lang.scala.{ScalaVerticle, VertxExecutionContext}
import io.vertx.pgclient._
import io.vertx.scala.core.http.{HttpServer, HttpServerRequest, HttpServerResponse}
import io.vertx.scala.core.{VertxOptions, _}
import io.vertx.scala.ext.web.Router
import io.vertx.sqlclient._
import vertx.model.{Fortune, Message, World}

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Sorting, Success, Try}

case class Header(name: CharSequence, value: String)

class App extends ScalaVerticle {

  private val HELLO_WORLD = "Hello, world!"
  private val HELLO_WORLD_BUFFER = Buffer.buffer(HELLO_WORLD, "UTF-8")
  private val SERVER = "vert.x"

  private val contentTypeJson = Header(HttpHeaders.CONTENT_TYPE, "application/json")
  private val contentTypeHtml = Header(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8")
  private val contentTypePlainText = Header(HttpHeaders.CONTENT_TYPE, "text/plain")

  private var dateString: String = ""

  private var server: HttpServer = _
  private var client: PgConnection = _

  private def refreshDateHeader(): Unit = dateString = App.createDateHeader()

  override def startFuture(): Future[_] = {
    refreshDateHeader()
    vertx.setPeriodic(1000, (_: Long) => refreshDateHeader())

    val pgConnectOptions = new PgConnectOptions()
      .setDatabase(config.getString("database"))
      .setHost(config.getString("host"))
      .setPort(config.getInteger("port", 5432))
      .setUser(config.getString("username"))
      .setPassword(config.getString("password"))
      .setCachePreparedStatements(true)
      .setPipeliningLimit(100000)

    val jVertx = vertx.asJava.asInstanceOf[io.vertx.core.Vertx]
    val pgConnectionPromise = Promise[Unit]
    PgConnection.connect(
      jVertx,
      pgConnectOptions,
      (ar => {
        client = ar.result()
        pgConnectionPromise.success()
      }): Handler[AsyncResult[PgConnection]]
    )

    val router = Router.router(vertx)
    router.get("/plaintext").handler(context => handlePlainText(context.request()))
    router.get("/json").handler(context => handleJson(context.request()))
    router.get("/db").handler(context => handleDb(context.request()))
    router.get("/queries").handler(context => handleQueries(context.request()))
    router.get("/updates").handler(context => handleUpdates(context.request()))
    router.get("/fortunes").handler(context => handleFortunes(context.request()))

    val port = 8080
    server = vertx.createHttpServer()
    val httpServerPromise = Promise[Unit]
    server
      .requestHandler(router.accept)
      .listen(port, (_ => httpServerPromise.success()): Handler[AsyncResult[HttpServer]])

    pgConnectionPromise.future.flatMap(_ => httpServerPromise.future)
  }

  override def stop(): Unit = Option(server).foreach(_.close())

  private def responseWithHeaders(response: HttpServerResponse, contentType: Header) =
    response
      .putHeader(contentType.name.toString, contentType.value)
      .putHeader(HttpHeaders.SERVER.toString, SERVER)
      .putHeader(HttpHeaders.DATE.toString, dateString)

  private def handlePlainText(request: HttpServerRequest): Unit = {
    responseWithHeaders(request.response, contentTypePlainText).end(HELLO_WORLD_BUFFER)
  }

  private def handleJson(request: HttpServerRequest): Unit =
    responseWithHeaders(request.response, contentTypeJson)
      .end(Message("Hello, World!").toBuffer)

  private def handleDb(request: HttpServerRequest): Unit =
    client
      .preparedQuery("SELECT id, randomnumber from WORLD where id=$1")
      .execute(
        Tuple.of(App.randomWorld(), Nil: _*),
        (ar: AsyncResult[RowSet[Row]]) => {
          if (ar.succeeded) {
            val resultSet = ar.result.iterator
            if (!resultSet.hasNext) {
              request.response.setStatusCode(404).end()
            } else {
              val row = resultSet.next
              responseWithHeaders(request.response, contentTypeJson)
                .end(World(row.getInteger(0), row.getInteger(1)).encode())
            }
          } else {
            App.logger.error("Failed to handle request", ar.cause())
            request.response.setStatusCode(500).end(ar.cause.getMessage)
          }
        }
      )

  private def handleQueries(request: HttpServerRequest): Unit = {
    val queries = App.getQueries(request)
    val worlds = new JsonArray
    var i = 0
    var failed = false
    while (i < queries) {
      client
        .preparedQuery("SELECT id, randomnumber from WORLD where id=$1")
        .execute(
          Tuple.of(App.randomWorld(), Nil: _*),
          (ar: AsyncResult[RowSet[Row]]) => {
            if (!failed) {
              if (ar.failed) {
                failed = true
                request.response.setStatusCode(500).end(ar.cause.getMessage)
                return
              }
              // we need a final reference
              val row = ar.result.iterator.next

              worlds.add(World(row.getInteger(0), row.getInteger(1)))
              if (worlds.size == queries)
                responseWithHeaders(request.response, contentTypeJson)
                  .end(worlds.encode)
            }
          }
        )

      i += 1
    }
  }

  private def handleUpdates(request: HttpServerRequest): Unit = {
    def sendError(err: Throwable): Unit = {
      App.logger.error("", err)
      request.response.setStatusCode(500).end(err.getMessage)
    }

    def handleUpdates(conn: SqlConnection, worlds: Array[World]): Unit = {
      Sorting.quickSort(worlds)

      val batch = worlds.map(world => Tuple.of(world.randomNumber, world.id)).toList.asJava
      conn
        .preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2")
        .executeBatch(
          batch,
          (ar: AsyncResult[RowSet[Row]]) => {
            if (ar.failed) {
              sendError(ar.cause)
              return
            }

            responseWithHeaders(request.response, contentTypeJson)
              .end(new JsonArray(worlds.toList.asJava).toBuffer)
          }
        )
    }

    val queries = App.getQueries(request)
    val worlds = new Array[World](queries)
    var failed = false
    var queryCount = 0

    var i = 0
    while (i < worlds.length) {
      val id = App.randomWorld()
      val index = i
      client
        .preparedQuery("SELECT id, randomnumber from WORLD where id=$1")
        .execute(
          Tuple.of(id, Nil: _*),
          (ar2: AsyncResult[RowSet[Row]]) => {
            if (!failed) {
              if (ar2.failed) {
                failed = true
                sendError(ar2.cause)
                return
              }
              worlds(index) = World(ar2.result.iterator.next.getInteger(0), App.randomWorld())
              queryCount += 1
              if (queryCount == worlds.length) handleUpdates(client, worlds)
            }
          }
        )

      i += 1
    }
  }

  private def handleFortunes(request: HttpServerRequest): Unit =
    client
      .preparedQuery("SELECT id, message from FORTUNE")
      .execute(
        (ar: AsyncResult[RowSet[Row]]) => {
          val response = request.response
          if (ar.succeeded) {
            val resultSet = ar.result.iterator
            if (!resultSet.hasNext) {
              response.setStatusCode(404).end("No results")
              return
            }
            val fortunes = (resultSet.asScala
              .map(row => Fortune(row.getInteger(0), row.getString(1))) ++
              Seq(Fortune(0, "Additional fortune added at request time."))).toArray
            Sorting.quickSort(fortunes)
            responseWithHeaders(request.response, contentTypeHtml)
              .end(html.fortune(fortunes).body)
          } else {
            val err = ar.cause
            App.logger.error("", err)
            response.setStatusCode(500).end(err.getMessage)
          }
        }
      )

}

object App {
  val logger: Logger = Logger[App]

  def main(args: Array[String]): Unit = {
    val config = new JsonObject(Files.readString(new File(args(0)).toPath))
    val vertx = Vertx.vertx(VertxOptions().setPreferNativeTransport(true))

    printConfig(vertx)

    vertx.exceptionHandler(_.printStackTrace())

    implicit val executionContext: VertxExecutionContext = VertxExecutionContext(vertx.getOrCreateContext())

    vertx
      .deployVerticleFuture(
        ScalaVerticle.nameForVerticle[App],
        DeploymentOptions().setInstances(JVertxOptions.DEFAULT_EVENT_LOOP_POOL_SIZE).setConfig(config)
      )
      .onComplete {
        case _: Success[String] => logger.info("Server listening on port 8080")
        case f: Failure[String] => logger.error("Unable to start application", f.exception)
      }
  }

  def createDateHeader(): String = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now)

  def randomWorld(): Int = 1 + ThreadLocalRandom.current.nextInt(10000)

  def getQueries(request: HttpServerRequest): Int =
    request
      .getParam("queries")
      .flatMap(param => Try(param.toInt).toOption)
      .map(number => Math.min(500, Math.max(1, number)))
      .getOrElse(1)

  private def printConfig(vertx: Vertx): Unit = {
    val version = Try {
      def resourceAsStream(resource: String) = classOf[Vertx].getClassLoader.getResourceAsStream(resource)

      val in =
        Option(resourceAsStream("META-INF/vertx/vertx-version.txt")).getOrElse(resourceAsStream("vertx-version.txt"))
      val out = new ByteArrayOutputStream
      val buffer = new Array[Byte](256)

      Iterator
        .continually(in.read(buffer))
        .takeWhile(_ != -1)
        .foreach(read => out.write(buffer, 0, read))

      out.toString
    }.recover {
      case e: IOException =>
        logger.error("Could not read Vert.x version", e)
        "unknown"
    }.get

    logger.info("Vert.x: {}", version)
    logger.info("Event Loop Size: {}", JVertxOptions.DEFAULT_EVENT_LOOP_POOL_SIZE)
    logger.info("Native transport: {}", vertx.isNativeTransportEnabled)
  }
}
