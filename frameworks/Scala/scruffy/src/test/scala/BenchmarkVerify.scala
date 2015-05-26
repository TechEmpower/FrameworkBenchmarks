import com.sksamuel.scruffy.client.ScruffyClient

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object BenchmarkVerify extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val client = ScruffyClient()

  val sw = Stopwatch()

  val futures = for ( k <- 1 to 30000 ) yield {
    client.get("http://localhost:8080/json").execute
  }

  val f = Future.sequence(futures)
  Await.result(f, 1.minute)

  sw.printMillis()

  client.close()
}

case class Stopwatch() {

  val start = System.currentTimeMillis

  def duration: FiniteDuration = {
    val end = System.currentTimeMillis
    (end - start).millis
  }

  def seconds: Long = duration.toSeconds
  def millis: Long = duration.toMillis

  def printMillis(): Unit = println(s"Time ${millis}ms")
}