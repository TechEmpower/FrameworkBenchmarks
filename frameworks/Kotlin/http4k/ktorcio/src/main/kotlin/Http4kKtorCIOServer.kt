import org.http4k.server.KtorCIO

fun main(args: Array<String>) {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(KtorCIO(9000))
}