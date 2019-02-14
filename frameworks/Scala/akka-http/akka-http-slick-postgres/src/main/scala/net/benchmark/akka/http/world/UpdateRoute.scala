package net.benchmark.akka.http.world

class UpdateRoute(wr: WorldRepository,
                  ud: ExecutionContextExecutor,
                  sd: ExecutionContextExecutor) {

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
    Source(1 to n)
      .map(rand)
      .mapAsync(n)(wr.require)
      .mapAsync(n) { w =>
        val wn = w.copy(randomNumber = rand())
        wr.update(wn).map(_ => wn)(sd)
      }
  }

  def route() = {
    path("updates") {
      parameter('queries.?) { pn =>
        complete(source(parse(pn)))
      }
    }
  }

}
