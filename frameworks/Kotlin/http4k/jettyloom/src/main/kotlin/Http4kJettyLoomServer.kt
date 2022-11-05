fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database"), false).start(JettyLoom(9000))
}