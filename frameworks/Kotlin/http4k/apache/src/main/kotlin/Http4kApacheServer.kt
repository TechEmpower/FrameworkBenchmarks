
import org.http4k.server.ApacheServer

fun main(args: Array<String>) {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(ApacheServer(9000))
}