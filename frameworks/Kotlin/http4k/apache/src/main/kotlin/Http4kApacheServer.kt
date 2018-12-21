
import org.http4k.server.ApacheServer

fun main(args: Array<String>) {
    Http4kBenchmarkServer().start(ApacheServer(9000))
}