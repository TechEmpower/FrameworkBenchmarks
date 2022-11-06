package http4k

import Database
import Fortune
import Http4kBenchmarkServer
import PostgresDatabase
import TfbApacheServer
import argo.jdom.JsonNode
import org.http4k.format.Argo.obj
import start

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(TfbApacheServer(9000))
}

object NoOpDatabase : Database {
    override fun findWorld() = obj()
    override fun loadAll() = emptyMap<Int, JsonNode>()
    override fun findWorlds(count: Int) = emptyList<JsonNode>()
    override fun updateWorlds(count: Int) = emptyList<JsonNode>()
    override fun fortunes() = emptyList<Fortune>()
}
