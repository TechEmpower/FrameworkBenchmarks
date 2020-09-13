package net.benchmark.akka.http.world
import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.Source
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

import scala.concurrent.ExecutionContextExecutor
import scala.util.Try

class UpdateRoute(wr: WorldRepository, sd: ExecutionContextExecutor) {

  implicit private val jss: JsonEntityStreamingSupport =
    EntityStreamingSupport.json().withParallelMarshalling(5, unordered = true)

  private def rand(i: Int): Int = {
    val _ = i
    java.util.concurrent.ThreadLocalRandom.current().nextInt(10000) + 1
  }

  private def rand(): Int = {
    java.util.concurrent.ThreadLocalRandom.current().nextInt(10000) + 1
  }

  private def parse(pn: Option[String]): Int = {
    pn.fold(Try(1))(s => Try(s.toInt)).getOrElse(1).min(500).max(1)
  }

  private def source(n: Int): Source[World, NotUsed] = {
    val t = if (1 <= n && n < 5) n else 5

    Source(1 to n)
      .map(rand)
      .mapAsync(t)(wr.require)
      .mapAsync(t) { w =>
        val wn = w.copy(randomNumber = rand())
        wr.update(wn).map(_ => wn)(sd)
      }
  }

  def route() = {
    path("updates") {
      parameter("queries".?) { pn: Option[String] =>
        complete(source(parse(pn)))
      }
    }
  }

}
