package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class World extends LongKeyedMapper[World] with IdPK {
    def getSingleton = World

    object randomNumber extends MappedLong(this)

}

object World extends World with LongKeyedMetaMapper[World] {
    override lazy val fieldOrder = List(id, randomNumber)

}

// vim: set ts=4 sw=4 et:
