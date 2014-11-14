package scruffy.examples

import com.sksamuel.scruffy.{ScruffyConfiguration, Scruffy}

/** @author Stephen Samuel */
object Main extends App {

  val port = 8080
  val scruffy = new Scruffy(ScruffyConfiguration.port(port).compression(false).requestLogging(false))
  scruffy.mount(new Test1Endpoint)
  scruffy.mount(new Test2Endpoint)
  scruffy.mount(new Test6Endpoint)
  println("Starting Scruffy...")
  val lifecycle = scruffy.start()
  println(s"Started on port [$port]. Interrupt to exit.")
  lifecycle.await()
}
