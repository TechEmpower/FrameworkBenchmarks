package database

import Fortune
import World
import org.jetbrains.exposed.v1.core.ResultRow
import org.jetbrains.exposed.v1.core.eq
import org.jetbrains.exposed.v1.jdbc.select as jdbcSelect
import org.jetbrains.exposed.v1.r2dbc.select as r2dbcSelect

fun jdbcSelectWorldWithIdQuery(id: Int) =
    WorldTable.jdbcSelect(WorldTable.id, WorldTable.randomNumber).where(WorldTable.id eq id)

fun r2dbcSelectWorldWithIdQuery(id: Int) =
    WorldTable.r2dbcSelect(WorldTable.id, WorldTable.randomNumber).where(WorldTable.id eq id)

fun ResultRow.toWorld() =
    World(this[WorldTable.id].value, this[WorldTable.randomNumber])

fun ResultRow.toFortune() =
    Fortune(this[FortuneTable.id].value, this[FortuneTable.message])
