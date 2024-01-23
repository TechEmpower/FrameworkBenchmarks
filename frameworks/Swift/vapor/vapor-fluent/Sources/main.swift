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


app.get("db") { req async throws -> World in
    guard let world = try await World.find(.random(in: 1...10_000), on: req.db) else {
        throw Abort(.notFound)
    }

    return world
}

app.get("queries") { req async throws -> [World] in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)

    var worlds: [World] = []

    for _ in queries {
        guard let world = try await World.find(.random(in: 1...10_000), on: req.db) else {
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
