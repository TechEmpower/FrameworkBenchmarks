import org.http4k.server.Undertow

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(Undertow(8080))
}