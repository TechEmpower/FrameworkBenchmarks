import VaporBridges
import Vapor

func routes(_ app: Application) throws {
    app.get { req async in
        "It works!"
    }

    app.get("hello") { req async -> String in
        "Hello, world!"
    }

    app.get("db") { req async throws -> World in
        guard let world = try await req.postgres.connection(to: .Db, { conn in
            World.select.where(\World.$id == Int.random(in: 1...10_000)).execute(on: conn).first(decoding: World.self)
        }).get() else {
            throw Abort(.notFound)
        }
        return world
    }
}
