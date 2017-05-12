package com.typesafe.akka.http.benchmark.util

import java.util.concurrent.ThreadLocalRandom

trait RandomGenerator {
  def nextRandomInt: Int = ThreadLocalRandom.current().nextInt() * 10000 + 1
}
