package benchmarks

import org.cognition._
import controllers._

object Benchmark {

  def main(args: Array[String]) {
    val server = new CognitionServer
    server.register(new JsonController)
    server.register(new DbController)
    server.register(new PlainController)
    server.register(new FortuneController)
    server.start
  }

}
