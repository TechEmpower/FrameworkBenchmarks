package app.model

import cc.otavia.json.JsonSerde
import cc.otavia.serde.annotation.rename
import cc.otavia.sql.{Row, RowCodec}

/** The model for the "world" database table. */
case class World(id: Int, randomNumber: Int) extends Row derives RowCodec, JsonSerde
