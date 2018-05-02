import Vapor
import Foundation
import TfbCommon
import FluentMySQL

struct EmptyJSON: Content {}

public func routes(_ router: Router) throws {

    router.get("json") { req in
        return Message("Hello, World!")
    }

    router.get("plaintext") { req in
        return "Hello, world!"
    }

    // response to requests to /info domain
    // with a description of the request
    router.get("info") { req in
        return req.description
    }

    router.get("description") { req in
        return req.description
    }

    // Test type 2: Single database query
    router.get("db") { req -> Future<Response> in
        let worldId = WorldMeta.randomId()
        return try World.find(worldId, on: req)
            .flatMap(to: Response.self) { world in
                guard let world = world
                    else { return try EmptyJSON().encode(for: req) }

                return try world.encode(for: req)
        }
    }

    // Test type 3: Multiple database queries
    router.get("queries") { req -> Future<[World]> in
        let queries = queriesParam(for: req)
        let ids = (1...queries).map({ _ in WorldMeta.randomId() })

        return try ids.map { id in
                return try World.find(id, on: req)
            }
            .flatten(on: req)
            .map(to: [World].self) { result in
                return result.compactMap({ $0 })
            }
    }

    // Test type 4: Fortunes
    router.get("fortunes") { req -> Future<View> in
        let posixLocale = Locale(identifier: "en_US_POSIX")

        return req.withPooledConnection(to: .mysql, closure: { (db: MySQLConnection) -> Future<[Fortune]> in
                return db.query(Fortune.self).all()
            })
            .map { fortunes -> [Fortune] in
                var newFortunes = fortunes
                let additional = Fortune(id: 0, message: "Additional fortune added at request time.")
                newFortunes.insert(additional, at: 0)
                newFortunes.sort(by: { lhs, rhs -> Bool in
                    return lhs.message.compare(rhs.message, locale: posixLocale) == .orderedAscending
                })
                return newFortunes
            }
            .flatMap { fortunes in
                return try req.view().render("fortune", fortunes)
            }

    }

    // Test type 5: Database updates
    router.get("updates") { req -> Future<[World]> in
        let queries = queriesParam(for: req)
        let ids = (1...queries).map({ _ in WorldMeta.randomId() })

        return try ids.map { id in
                return try World.find(id, on: req)
            }
            .flatten(on: req)
            .map(to: [World].self) { result in
                let worlds = result.compactMap({ $0 })
                worlds.forEach { $0.randomNumber = WorldMeta.randomRandomNumber() }
                return worlds
            }
            .flatMap(to: [World].self) { worlds in
                return worlds
                    .map { $0.save(on: req) }
                    .flatten(on: req)
            }.flatMap { _ in
                return req.withPooledConnection(to: .mysql, closure: { db -> Future<[World]> in
                    return db.query(World.self).all()
                })
            }
    }
}
