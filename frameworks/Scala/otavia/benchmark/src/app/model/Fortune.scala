package app.model

import cc.otavia.json.JsonSerde
import cc.otavia.sql.{Row, RowCodec}

/** The model for the "fortune" database table. */
case class Fortune(id: Int, message: String) extends Row derives RowCodec, JsonSerde
