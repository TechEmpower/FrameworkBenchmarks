package pekko.http.benchmark

import pekko.http.benchmark


object Main {
  def main(args: Array[String]): Unit =
    (new benchmark.App).run()
}