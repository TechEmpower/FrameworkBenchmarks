import org.http4k.server.Netty

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(Netty(9000))
}