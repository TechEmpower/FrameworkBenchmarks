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
    func sql(_ pools: EventLoopGroupConnectionPool<PostgresConnectionSource>) -> SQLDatabase {
        pools.pool(for: self.eventLoop).database(logger: self.logger).sql()
    }
}

app.get("db") { req async throws -> World in
    guard let world = try await req.sql(pools).select()
        .column("id")
        .column("randomnumber")
        .from("World")
        .where("id", .equal, Int32.random(in: 1...10_000))
        .first(decoding: World.self)
        .get() 
        else {  
            throw Abort(.notFound)
        }

    return world
}

app.get("queries") { req async throws -> [World] in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)

    var worlds: [World] = []

    let db = req.sql(pools)

    for _ in queries {
        guard let world = try await db.select()
            .column("id")
            .column("randomnumber")
            .from("World")
            .where("id", .equal, Int32.random(in: 1...10_000))
            .first(decoding: World.self).get() else {
            throw Abort(.notFound)
        }

        worlds.append(world)
    }

    return worlds
}

extension Int: Sequence {
    public func makeIterator() -> CountableRange<Int>.Iterator {
        return (0..<self).makeIterator()
    }
}

try app.run()
