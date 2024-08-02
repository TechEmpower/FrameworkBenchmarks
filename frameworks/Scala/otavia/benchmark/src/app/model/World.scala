package app.model

import cc.otavia.json.JsonSerde
import cc.otavia.serde.annotation.rename
import cc.otavia.sql.{Row, RowDecoder}

/** The model for the "world" database table. */
case class World(id: Int, @rename("randomnumber") randomNumber: Int) extends Row derives RowDecoder, JsonSerde
