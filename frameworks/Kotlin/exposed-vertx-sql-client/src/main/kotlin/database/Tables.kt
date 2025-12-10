package database

import org.jetbrains.exposed.v1.core.dao.id.IdTable

// see "toolset/databases/postgres/create-postgres.sql"

object WorldTable : IdTable<Int>("World") {
    override val id = integer("id").entityId()

    // The name is "randomNumber" in "create-postgres.sql" but it's actually "randomnumber" in the test database.
    val randomNumber = integer("randomnumber").default(0)
}

object FortuneTable : IdTable<Int>("Fortune") {
    override val id = integer("id").entityId()
    val message = varchar("message", 2048)
}
