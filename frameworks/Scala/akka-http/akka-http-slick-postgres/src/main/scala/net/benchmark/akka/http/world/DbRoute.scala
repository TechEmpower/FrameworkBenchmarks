package net.benchmark.akka.http.world

class DbRoute(wr: WorldRepository, dd: ExecutionContextExecutor) {

  private def rand(): Int = {
    java.util.concurrent.ThreadLocalRandom.current().nextInt(10000) + 1
  }

  def route() = {
    path("db") {
      complete(wr.require(rand()))
    }
  }

}
