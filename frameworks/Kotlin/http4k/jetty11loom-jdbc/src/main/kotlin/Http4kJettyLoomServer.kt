import org.http4k.server.Jetty11Loom

fun main() {
    Http4kBenchmarkServer(PostgresDatabase(), false).start(Jetty11Loom(9000))
}