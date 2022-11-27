import org.http4k.server.Netty

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(Netty(9000))
}