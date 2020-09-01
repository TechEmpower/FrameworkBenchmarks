import org.http4k.server.Apache4Server

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(Apache4Server(9000))
}