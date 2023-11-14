import org.http4k.server.KtorNetty

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(KtorNetty(9000))
}