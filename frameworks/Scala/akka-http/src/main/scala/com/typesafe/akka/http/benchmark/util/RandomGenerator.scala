package com.typesafe.akka.http.benchmark.util

class RandomGenerator(components: {

}) {

  def next: Int = {
    (Math.random() * 10000 + 1).toInt
  }
}
