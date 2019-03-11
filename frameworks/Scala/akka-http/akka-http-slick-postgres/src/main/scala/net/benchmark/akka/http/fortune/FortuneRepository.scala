package net.benchmark.akka.http.fortune
import slick.basic.DatabasePublisher

trait FortuneRepository {

  def all(): DatabasePublisher[Fortune]

}
