
import org.http4k.server.ApacheServer

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(ApacheServer(9000))
}