import org.http4k.server.Jetty

fun main() {
    Http4kBenchmarkServer(PostgresDatabase(), false).start(Jetty(9000))
}