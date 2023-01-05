import org.http4k.server.Apache4Server

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(Apache4Server(9000))
}