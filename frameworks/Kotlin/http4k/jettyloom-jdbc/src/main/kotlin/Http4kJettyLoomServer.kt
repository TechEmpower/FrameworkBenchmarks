fun main() {
    Http4kBenchmarkServer(PostgresDatabase(), false).start(JettyLoom(9000))
}