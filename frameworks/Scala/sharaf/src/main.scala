package ba.sake.sharaf.benchmark

import io.undertow.Undertow
import io.undertow.UndertowOptions
import ba.sake.sharaf.*

@main def run(): Unit = {
  val dao = DAO()
  val benchmarkRoutes = BenchmarkRoutes(dao)
  // set to slf4j, thus disabling logging (slf4j-nop)
  System.setProperty("org.jboss.logging.provider", "slf4j")
  val server = Undertow
    .builder()
    .addHttpListener(8080, "0.0.0.0")
    .setHandler(SharafHandler(benchmarkRoutes.routes))
    .setIoThreads(Runtime.getRuntime().availableProcessors() * 2)
    // In HTTP/1.1, connections are persistent unless declared otherwise.
    // Adding a "Connection: keep-alive" header to every response would only
    // add useless bytes.
    .setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
    .build()
  server.start()
  //println(s"Started HTTP server at localhost:8080")
}
