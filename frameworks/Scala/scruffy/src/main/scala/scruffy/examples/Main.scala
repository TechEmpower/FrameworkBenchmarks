package scruffy.examples

import com.sksamuel.scruffy.ScruffyContext
import com.sksamuel.scruffy.undertow.ScruffyUndertowHttpHandler
import io.undertow.{Undertow, UndertowOptions}

/** @author Stephen Samuel */
object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val port = 8080

  val context = ScruffyContext(Test1Endpoint, Test2Endpoint, Test6Endpoint)
  val handler = new ScruffyUndertowHttpHandler(context)

  val server = Undertow
    .builder()
    .addHttpListener(port, "0.0.0.0")
    .setHandler(handler)
    .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, java.lang.Boolean.FALSE)
    .setServerOption(UndertowOptions.ALWAYS_SET_DATE, java.lang.Boolean.TRUE)
    .setServerOption(UndertowOptions.ENABLE_CONNECTOR_STATISTICS, java.lang.Boolean.FALSE)
    .setServerOption(UndertowOptions.MAX_CONCURRENT_REQUESTS_PER_CONNECTION, Integer.valueOf(8))

    .build()

  println("Starting Scruffy on Undertow...")
  server.start()
  println(s"Started on port [$port]. Interrupt to exit.")
}
