import org.http4k.server.Jetty

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database"), false).start(Jetty(8080))
}