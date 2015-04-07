package scruffy.examples

import com.sksamuel.scruffy.undertow.ScruffyUndertowHttpHandler
import io.undertow.{UndertowOptions, Undertow}

/** @author Stephen Samuel */
object Main extends App {

  val port = 8080

  val scruffy = new ScruffyUndertowHttpHandler
  scruffy.mount(new Test1Endpoint)
  scruffy.mount(new Test2Endpoint)
  scruffy.mount(new Test6Endpoint)

  val server = Undertow
    .builder()
    .addHttpListener(port, "localhost")
    .setHandler(scruffy)
    .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, java.lang.Boolean.FALSE)
    .setServerOption(UndertowOptions.ALWAYS_SET_DATE, java.lang.Boolean.TRUE)
    .setServerOption(UndertowOptions.ENABLE_CONNECTOR_STATISTICS, java.lang.Boolean.FALSE)
    .setServerOption(UndertowOptions.MAX_CONCURRENT_REQUESTS_PER_CONNECTION, Integer.valueOf(8))

    .build()

  println("Starting Undertow...")
  server.start()
  println(s"Started on port [$port]. Interrupt to exit.")
}
