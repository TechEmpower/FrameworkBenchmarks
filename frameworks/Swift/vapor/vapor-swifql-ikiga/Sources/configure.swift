//import IkigaJSON
import FoundationPreview
import PostgresBridge
import Vapor
import VaporBridges


extension DatabaseHost {
    public static var DbHost: DatabaseHost {
        return .init(
            hostname: "tfb-database",
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
        let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)

        var worlds: [World] = []

        for _ in queries {
            
        }
    }
}
