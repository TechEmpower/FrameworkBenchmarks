package database

import Fortune
import World
import org.jetbrains.exposed.v1.core.ResultRow
import org.jetbrains.exposed.v1.core.eq
import org.jetbrains.exposed.v1.jdbc.select

fun selectWorldWithIdQuery(id: Int) =
    WorldTable.select(WorldTable.id, WorldTable.randomNumber).where(WorldTable.id eq id)

fun ResultRow.toWorld() =
    World(this[WorldTable.id].value, this[WorldTable.randomNumber])

fun ResultRow.toFortune() =
    Fortune(this[FortuneTable.id].value, this[FortuneTable.message])
