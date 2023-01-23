import org.http4k.server.Undertow

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(Undertow(9000))
}