
import FoundationPreview
import Leaf
import PostgresBridge
import Vapor
import VaporBridges


extension DatabaseHost {
    public static var DbHost: DatabaseHost {
        return .init(
            hostname: "localhost",
            port: 5432,
            username: "benchmarkdbuser",
            password: "benchmarkdbpassword",
            tlsConfiguration: nil
        )
    }
}

extension DatabaseIdentifier {
    public static var Db: DatabaseIdentifier {
        .init(name: "hello_world", host: .DbHost)
    }
}


public func configure(_ app: Application) throws {
    let decoder = FoundationEssentials.JSONDecoder()
    decoder.dateDecodingStrategy = .iso8601
    ContentConfiguration.global.use(decoder: decoder as ContentDecoder, for: .json)

    let encoder = FoundationEssentials.JSONEncoder()
    encoder.dateEncodingStrategy = .iso8601
    ContentConfiguration.global.use(encoder: encoder as ContentEncoder, for: .json)

    app.http.server.configuration.serverName = "Vapor"
    app.logger.logLevel = .notice
    app.logger.notice("💧 VAPOR")
    app.logger.notice("System.coreCount: \(System.coreCount)")

    app.views.use(.leaf)
    
    try routes(app)
    
    try app.run()
}

public func routes(_ app: Application) throws {
    app.get("plaintext") { req async in
        "Hello, world!"
    }

    app.get("json") { req async in
        ["message": "Hello, world!"]
    }

    app.get("db") { req async throws -> World in
        guard let world: World = try await req.postgres.connection(to: .Db, { conn in
            World.select
                .where(\World.$id == Int.random(in: 1...10_000))
                .execute(on: conn)
                .first(decoding: World.self) 
        }).get() else {
            throw Abort(.notFound)
        }
        return world
    }
    
    app.get("queries") { req async throws -> [World] in
        let queries: Int = (req.query["queries"] ?? 1).bounded(to: 1...500)

        var worlds: [World] = []

        for _ in queries {
            guard let world: World = try await req.postgres.connection(to: .Db, { conn in
                World.select
                    .where(\World.$id == Int.random(in: 1...10_000))
                    .execute(on: conn)
                    .first(decoding: World.self) 
            }).get() else {
                throw Abort(.notFound)
            }

        worlds.append(world)
        }
        return worlds
    }

    app.get("updates") { req async throws -> [World] in
        let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)

        var worlds: [World] = []

        for _ in queries {
            let world = try await req.postgres.connection(to: .Db, { conn in
                    World.select.where(\World.$id == Int.random(in: 1...10_000)).execute(on: conn).first(decoding: World.self).flatMap { world in
                        world!.randomnumber = .random(in: 1...10_000)
                        return world!.update(on: \.$id, on: conn)
                }
            }).get()

            worlds.append(world)
        }

        return worlds
        
    }

    app.get("fortunes") { req async throws -> View in 
        var fortunes: [Fortune] = try await req.postgres.connection(to: .Db, {conn in 
            Fortune.select.execute(on: conn).all(decoding: Fortune.self)
        }).get()

        fortunes.append(Fortune(id: 0, message: "Additional fortune added at request time."))

        return try await req.view.render("fortune", fortunes)
    }
}

