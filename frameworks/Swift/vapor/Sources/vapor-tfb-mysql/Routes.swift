import Vapor
import TfbCommon
import Foundation

final class Routes: RouteCollection {
    func build(_ builder: RouteBuilder) throws {
        builder.get("json") { req in
            return try JSON(node: Message("Hello, World!"))
        }

        builder.get("plaintext") { req in
            return "Hello, world!"
        }

        // response to requests to /info domain
        // with a description of the request
        builder.get("info") { req in
            return req.description
        }

        builder.get("description") { req in return req.description }

        // Test type 2: Single database query
        builder.get("db") { _ in
            let worldId = WorldMeta.randomId()
            return try World.find(worldId)?.makeJSON() ?? JSON(node: .null)
        }

        // Test type 3: Multiple database queries
        builder.get("queries") { req in
            let queries = queriesParam(for: req)
            let ids = (1...queries).map({ _ in WorldMeta.randomId() })
            let worlds = try ids.flatMap { try World.find($0)?.makeJSON() }
            return JSON(worlds)
        }

        // Test type 4: Fortunes
        let posixLocale = Locale(identifier: "en_US_POSIX")
        builder.get("fortunes") { _ in
            var fortunes = try Fortune.all()
            let additional = Fortune(id: 0, message: "Additional fortune added at request time.")
            fortunes.insert(additional, at: 0)
            fortunes.sort(by: { lhs, rhs -> Bool in
                return lhs.message.compare(rhs.message, locale: posixLocale) == .orderedAscending
            })

            let nodes = try fortunes.map { try $0.makeJSONNode() }
            return try drop.view.make("fortune", ["fortunes": Node(nodes)])
        }

        // Test type 5: Database updates
        builder.get("updates") { req in
            let queries = queriesParam(for: req)
            let ids = (1...queries).map({ _ in WorldMeta.randomId() })
            var worlds = try ids.flatMap { try World.find($0) }
            worlds.forEach { $0.randomNumber = WorldMeta.randomRandomNumber() }
            worlds = try worlds.flatMap { world in
                let modifiedWorld = world
                try modifiedWorld.save()
                return modifiedWorld
            }
            let updatedWorlds = try worlds.flatMap { try $0.makeJSON() }
            return JSON(updatedWorlds)
        }
    }
}


/// Since Routes doesn't depend on anything
/// to be initialized, we can conform it to EmptyInitializable
///
/// This will allow it to be passed by type.
extension Routes: EmptyInitializable { }
