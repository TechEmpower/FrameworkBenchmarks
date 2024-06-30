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

// Plaintext
app.get("plaintext") { req async in 
    return "Hello, World!"
}

// JSON
app.get("json") { req async in
    return ["messgage": "Hello, World!"]
}

// Fortunes
app.get("fortunes") { req async throws -> Fortune in 
    let rows = try await req.db(pools).query("SELECT id, message FROM Fortune")
    rows.append(["id": 0, "message": "Additional fortune added at request time."])
}


// Single query
app.get("db") { req async throws -> World in
    let rows = try await req.db(pools).query("SELECT id, randomnumber FROM World WHERE id = $1", [
        PostgresData(int32: .random(in: 1...10_000))]).get()
        
    if (rows.count == 0) {  
           throw Abort(.notFound)
    }

    let world =  World(
            id: rows[0].column("id")?.int32 ?? 0,
            randomnumber: rows[0].column("randomnumber")?.int ?? 0
        )

    return world
}

// Multiple queries
app.get("queries") { req async throws -> [World] in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)

    var worlds: [World] = []

    for _ in queries {
        let rows = try await req.db(pools).query("SELECT id, randomnumber FROM World WHERE id = $1", [
            PostgresData(int32: .random(in: 1...10_000))]).get()
            
        if (rows.count == 0) {  
            throw Abort(.notFound)
        }

        let world =  World(
                id: rows[0].column("id")?.int32 ?? 0,
                randomnumber: rows[0].column("randomnumber")?.int ?? 0
            )

        worlds.append(world)
    }

    return worlds
}

// Database Updates test
//
app.get("updates") { req async throws -> [World] in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)

    var worlds: [World] = []

    let db = req.db(pools)

    for _ in queries {
        // Get
        //
        let rows = try await db.query(
            "SELECT id, randomnumber FROM World WHERE id = $1 LIMIT 1",
            [PostgresData(int32: .random(in: 1...10_000))]
        ).get()

        guard let row = rows.first else {
            throw Abort(.notFound)
        }

        var world =  World(
            id: row.column("id")!.int32!,
            randomnumber: row.column("randomnumber")!.int!
        )

        // Update
        //
        world.randomnumber = .random(in: 1...10_000)

        _ = try await db.query(
            "UPDATE World SET randomnumber = $1 WHERE id = $2",
            [PostgresData(int: world.randomnumber), PostgresData(int32: world.id!)]
        ).get()

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
