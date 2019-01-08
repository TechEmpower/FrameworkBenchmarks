
import org.http4k.server.Undertow

fun main(args: Array<String>) {
    Http4kBenchmarkServer(BlockingDatabase("tfb-database")).start(Undertow(9000))
}