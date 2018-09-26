import Logging
import PostgreSQL
import Vapor

struct BadRequestError : AbortError {
    var status: HTTPResponseStatus {
        return .badRequest
    }

    var reason: String {
        return "Bad request"
    }

    var identifier: String {
        return "badRequest"
    }

}

final class WorldController {

    func get(_ req: Request) throws -> Future<World> {
        return req.withPooledConnection(to: .psql) { (conn: PostgreSQLDatabase.Connection) in
            return conn.select().all().from(World.self).where(\World.id == Int.random(in: 1...10_000)).all(decoding: World.self)
        }.map { (rows: [World]) in
            if let first = rows.first {
                return first
            }
            throw NotFound(rootCause: nil)
        }
    }

    func queries(_ req: Request) throws -> Future<[World]> {
        let numQueries: Int
        do {
            let queries = try req.query.get(Int.self, at: "queries")
            if queries < 1 {
                numQueries = 1
            } else if queries > 500 {
                numQueries = 500
            } else {
                numQueries = queries
            }
        } catch {
            numQueries = 1
        }
        var futures: [Future<World>] = []
        for _ in 0..<numQueries {
            futures.append(
                req.withPooledConnection(to: .psql) { (conn: PostgreSQLDatabase.Connection) in
                    return conn.select().all().from(World.self).where(\World.id == Int.random(in: 1...10_000)).all(decoding: World.self)
                }.map { (rows: [World]) in
                    if let first = rows.first {
                        return first
                    }
                    throw NotFound(rootCause: nil)
                }
            )
        }
        return Future<[World]>.reduce(into: [], futures, eventLoop: req.eventLoop) { (worlds, world) in
            return worlds.append(world)
        }
    }

}
