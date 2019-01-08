
import org.http4k.server.Netty

fun main(args: Array<String>) {
    Http4kBenchmarkServer(BlockingDatabase("tfb-database")).start(Netty(9000))
}