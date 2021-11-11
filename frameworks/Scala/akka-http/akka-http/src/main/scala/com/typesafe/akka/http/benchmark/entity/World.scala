package com.typesafe.akka.http.benchmark.entity

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class World(id: Int, randomNumber: Int)

object World {
  implicit val codec: JsonValueCodec[World] = JsonCodecMaker.make[World](CodecMakerConfig)
  implicit val seqCodec: JsonValueCodec[Seq[World]] = JsonCodecMaker.make[Seq[World]](CodecMakerConfig)
}
