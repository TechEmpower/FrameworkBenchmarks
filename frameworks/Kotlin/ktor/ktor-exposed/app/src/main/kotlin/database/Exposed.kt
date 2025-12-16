package database

import Fortune
import World
import org.jetbrains.exposed.v1.core.dao.id.EntityID
import org.jetbrains.exposed.v1.core.dao.id.IdTable
import org.jetbrains.exposed.v1.dao.IntEntity
import org.jetbrains.exposed.v1.dao.IntEntityClass

// see "toolset/databases/postgres/create-postgres.sql"

object WorldTable : IdTable<Int>("World") {
    override val id = integer("id").entityId()
    val randomNumber = integer("randomnumber").default(0) // The name is "randomNumber" in "create-postgres.sql".
}

object FortuneTable : IdTable<Int>("Fortune") {
    override val id = integer("id").entityId()
    val message = varchar("message", 2048)
}


class WorldDao(id: EntityID<Int>) : IntEntity(id) {
    companion object : IntEntityClass<WorldDao>(WorldTable)

    var randomNumber by WorldTable.randomNumber
    fun toWorld() =
        World(id.value, randomNumber)
}

class FortuneDao(id: EntityID<Int>) : IntEntity(id) {
    companion object : IntEntityClass<FortuneDao>(FortuneTable)

    var message by FortuneTable.message
    fun toFortune() =
        Fortune(id.value, message)
}
