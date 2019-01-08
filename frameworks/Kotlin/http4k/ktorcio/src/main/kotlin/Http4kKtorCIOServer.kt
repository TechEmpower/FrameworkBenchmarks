import org.http4k.server.KtorCIO

fun main(args: Array<String>) {
    Http4kBenchmarkServer(BlockingDatabase("tfb-database")).start(KtorCIO(9000))
}