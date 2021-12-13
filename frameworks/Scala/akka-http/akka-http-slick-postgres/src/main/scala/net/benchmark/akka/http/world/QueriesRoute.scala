package net.benchmark.akka.http.world
import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

import scala.util.Try

class QueriesRoute(wr: WorldRepository) {

  implicit private val jss: JsonEntityStreamingSupport =
    EntityStreamingSupport.json().withParallelMarshalling(5, unordered = true)

  private def rand(i: Int): Int = {
    val _ = i
    java.util.concurrent.ThreadLocalRandom.current().nextInt(10000) + 1
  }

  private def parse(pn: Option[String]): Int = {
    pn.fold(Try(1))(s => Try(s.toInt)).getOrElse(1).min(500).max(1)
  }

  private def source(n: Int): Source[World, NotUsed] = {
    val t = if (1 <= n && n < 5) n else 5

    Source(1 to n)
      .map(rand)
      .mapAsync(t)(i => wr.require(i))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def route(): Route = {
    path("queries") {
      parameter("queries".?) { pn: Option[String] =>
        complete {
          source(parse(pn))
        }
      }
    }
  }

}
