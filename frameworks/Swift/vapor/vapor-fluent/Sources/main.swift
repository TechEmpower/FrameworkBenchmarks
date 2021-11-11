import Fluent
import FluentPostgresDriver
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)

let app = Application(env)
defer { app.shutdown() }

app.http.server.configuration.serverName = "Vapor"

app.logger.notice("💧 VAPOR")
app.logger.notice("System.coreCount: \(System.coreCount)")
app.logger.notice("System.maxConnectionsPerEventLoop: \(System.maxConnectionsPerEventLoop)")

app.databases.use(.postgres(
    hostname: "tfb-database",
    username: "benchmarkdbuser",
    password: "benchmarkdbpass",
    database: "hello_world",
    maxConnectionsPerEventLoop: System.maxConnectionsPerEventLoop
), as: .psql)

app.get("plaintext") { req in
    "Hello, world!"
}

app.get("json") { req in
    ["message": "Hello, world!"]
}

app.get("db") { req in
    World.find(.random(in: 1...10_000), on: req.db)
        .unwrap(or: Abort(.notFound))
}

app.get("queries") { req -> EventLoopFuture<[World]> in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)
    return (0 ..< queries).map { _ -> EventLoopFuture<World> in
        World.find(.random(in: 1...10_000), on: req.db)
            .unwrap(or: Abort(.notFound))
    }.flatten(on: req.eventLoop)
}

try app.run()
