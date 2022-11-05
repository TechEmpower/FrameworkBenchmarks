import org.http4k.server.KtorCIO

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(KtorCIO(8080))
}