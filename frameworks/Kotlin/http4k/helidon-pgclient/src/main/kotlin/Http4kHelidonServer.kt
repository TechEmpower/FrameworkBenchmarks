import org.http4k.server.Helidon

fun main() {
    Http4kBenchmarkServer(PostgresDatabase(), true).start(Helidon(9000))
}