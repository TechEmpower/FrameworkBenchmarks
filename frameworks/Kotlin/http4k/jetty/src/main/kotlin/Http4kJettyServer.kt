
import org.http4k.server.Jetty

fun main(args: Array<String>) {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database"), false).start(Jetty(9000))
}