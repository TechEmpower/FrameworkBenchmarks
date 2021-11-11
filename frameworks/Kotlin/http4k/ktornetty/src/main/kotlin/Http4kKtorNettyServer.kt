import org.http4k.server.KtorNetty

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(KtorNetty(9000))
}