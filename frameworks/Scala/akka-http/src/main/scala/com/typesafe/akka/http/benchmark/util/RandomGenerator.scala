package com.typesafe.akka.http.benchmark.util

import java.util.concurrent.ThreadLocalRandom

trait RandomGenerator {
  def nextRandomIntBetween1And10000: Int =
    ThreadLocalRandom.current().nextInt(10000) + 1
}
