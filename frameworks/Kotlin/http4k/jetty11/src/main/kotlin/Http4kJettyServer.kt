import org.http4k.server.Jetty11

fun main() {
    Http4kBenchmarkServer(PostgresDatabase(), false).start(Jetty11(9000))
}