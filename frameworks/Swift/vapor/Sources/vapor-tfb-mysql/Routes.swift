import Vapor
import Foundation
import TfbCommon
import FluentMySQL

struct EmptyJSON: Content {}

public func routes(_ router: Router) throws {

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
    router.get("queries", String.parameter) { req -> Future<[World]> in
        let queries = try queriesParam(for: req)
        let ids = (1...queries).map({ _ in WorldMeta.randomId() })

        return try World.query(on: req)
            .filter(\.id, .in, .array(ids))
            .all()
    }

    // Test type 4: Fortunes
    router.get("fortunes") { req -> Future<View> in
        let posixLocale = Locale(identifier: "en_US_POSIX")

        return Fortune.query(on: req).all()
            .map { fortunes -> [Fortune] in
                var newFortunes = fortunes
                let additional = Fortune(id: 0, message: "Additional fortune added at request time.")
                newFortunes.append(additional) // example used insert(, at: 0)
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
    router.get("updates", String.parameter) { req -> Future<[World]> in
        let queries = try queriesParam(for: req)
        let ids = (1...queries).map({ _ in WorldMeta.randomId() })

        return try ids.map { id in
                return try World.find(id, on: req)
            }
            .flatten(on: req)
            .map { result -> [World] in
                let worlds = result.compactMap({ $0 })
                worlds.forEach { $0.randomNumber = WorldMeta.randomRandomNumber() }
                return worlds
            }
            .flatMap { worlds -> Future<[World]> in
                return worlds
                    .map { $0.save(on: req) }
                    .flatten(on: req)
            }.flatMap { _ in
                return World.query(on: req).all()
            }
    }
}
