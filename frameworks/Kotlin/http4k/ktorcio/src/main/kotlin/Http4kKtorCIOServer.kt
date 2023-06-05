import org.http4k.server.KtorCIO

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(KtorCIO(9000))
}