import Hummingbird
import PostgresKit

// tfb-server (aka, citrine) uses 28 hyper-threaded cores
// postgresql.conf specifies max_connections = 2000
//
// 2000 / (28 * 2) = 35.7 (theoretical max)
//
// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Environment#citrine-self-hosted
// https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/toolset/databases/postgres/postgresql.conf#L64
let maxConnectionsPerEventLoop = 32
var connectionPool: EventLoopGroupConnectionPool<PostgresConnectionSource>!

extension HBApplication {
    func initConnectionPool() {
        connectionPool = EventLoopGroupConnectionPool(
            source: PostgresConnectionSource(configuration: .init(
                hostname: "tfb-database",
                username: "benchmarkdbuser",
                password: "benchmarkdbpass",
                database: "hello_world"
            )),
            maxConnectionsPerEventLoop: maxConnectionsPerEventLoop,
            on: self.eventLoopGroup
        )
    }
}

extension HBRequest {
    var db: PostgresDatabase {
        connectionPool.pool(for: self.eventLoop).database(logger: self.logger)
    }
}
