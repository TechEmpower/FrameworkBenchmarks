package http4k

import Http4kBenchmarkServer
import PostgresDatabase
import TfbApacheServer
import start

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(TfbApacheServer(9000))
}
