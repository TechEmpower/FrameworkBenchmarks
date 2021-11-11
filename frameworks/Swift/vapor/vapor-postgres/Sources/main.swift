import PostgresKit
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)

let app = Application(env)
defer { app.shutdown() }

app.http.server.configuration.serverName = "Vapor"

app.logger.notice("💧 VAPOR")
app.logger.notice("System.coreCount: \(System.coreCount)")
app.logger.notice("System.maxConnectionsPerEventLoop: \(System.maxConnectionsPerEventLoop)")

let pools = EventLoopGroupConnectionPool(
    source: PostgresConnectionSource(configuration: .init(
        hostname: "tfb-database",
        username: "benchmarkdbuser",
        password: "benchmarkdbpass",
        database: "hello_world"
    )), 
    maxConnectionsPerEventLoop: System.maxConnectionsPerEventLoop,
    on: app.eventLoopGroup
)

extension Request {
    func db(_ pools: EventLoopGroupConnectionPool<PostgresConnectionSource>) -> PostgresDatabase {
        pools.pool(for: self.eventLoop).database(logger: self.logger)
    }
}

app.get("db") { req in
    req.db(pools).query("SELECT id, randomnumber FROM World WHERE id = $1", [
        PostgresData(int32: .random(in: 1...10_000))
    ]).map {
        $0.first
    }.unwrap(or: Abort(.notFound)).map {
        World(
            id: $0.column("id")?.int32 ?? 0,
            randomnumber: $0.column("randomnumber")?.int ?? 0
        )
    }
}

app.get("queries") { req -> EventLoopFuture<[World]> in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)
    let db = req.db(pools)
    return (0 ..< queries).map { _ -> EventLoopFuture<World> in
        db.query("SELECT id, randomnumber FROM World WHERE id = $1", [
            PostgresData(int32: .random(in: 1...10_000))
        ]).map {
            $0.first
        }.unwrap(or: Abort(.notFound)).map {
            World(
                id: $0.column("id")?.int32 ?? 0,
                randomnumber: $0.column("randomnumber")?.int ?? 0
            )
        }
    }.flatten(on: req.eventLoop)
}

try app.run()
