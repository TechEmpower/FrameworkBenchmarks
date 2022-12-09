package http4k

import Http4kBenchmarkServer
import PostgresDatabase
import org.http4k.server.SunHttp
import start

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(SunHttp(9000))
}
