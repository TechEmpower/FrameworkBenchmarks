import Foundation
import Vapor
import JSON
import HTTP
import PostgreSQLProvider
import TfbCommon

import Node

final class MyContext: Context {
}

let myContext = MyContext()

extension Context {
    var isMyContext: Bool {
        return self is MyContext
    }
}

let config = try Config()
try config.addProvider(PostgreSQLProvider.Provider.self)
// All test types require `Server` and `Date` HTTP response headers.
// Vapor has standard middleware that adds `Date` header.
// We use custom middleware that adds `Server` header.

config.addConfigurable(middleware: ServerMiddleware(), name: "server-middleware")

let drop = try Droplet(config)

// Normally we would add preparation for Fluent Models.
//   `drop.preparations.append(World.self)` etc.
// During preparation Fluent creates `fluent` table to track migrations.
// But TFB environment does not grant user rights to create tables.
// So we just configure our Models with correct database.
World.database = drop.database
Fortune.database = drop.database

// Test type 1: JSON serialization
drop.get("json") { req in
    return try JSON(node: Message("Hello, World!"))
}

// Test type 2: Single database query
drop.get("db") { _ in
    let worldId = WorldMeta.randomId()
    
    return try World.find(worldId)?.makeJSON() ?? JSON(node: .null)
}

// Test type 3: Multiple database queries
drop.get("queries") { req in
    let queries = queriesParam(for: req)
    let ids = (1...queries).map({ _ in WorldMeta.randomId() })
    let worlds = try ids.flatMap { try World.find($0)?.makeJSON() }
    return JSON(worlds)
}

// Test type 4: Fortunes
private let posixLocale = Locale(identifier: "en_US_POSIX")
drop.get("fortunes") { _ in
    var fortunes = try Fortune.all()
    let additional = Fortune(id: 0, message: "Additional fortune added at request time.")
    fortunes.insert(additional, at: 0)
    fortunes.sort(by: { lhs, rhs -> Bool in
        return lhs.message.compare(rhs.message, locale: posixLocale) == .orderedAscending
    })
    
    let nodes = try fortunes.map { try $0.makeNode(context: myContext) }
    return try drop.view.make("fortune", ["fortunes": Node(nodes)])
}

// Test type 5: Database updates
drop.get("updates") { req in
    let queries = queriesParam(for: req)
    let ids = (1...queries).map({ _ in WorldMeta.randomId() })
    var worlds = try ids.flatMap { try World.find($0) }
    worlds.forEach { $0.randomNumber = WorldMeta.randomRandomNumber() }
    worlds = try worlds.flatMap { world in
        var modifiedWorld = world
        try modifiedWorld.save()
        return modifiedWorld
    }
    let updatedWorlds = try worlds.flatMap { try $0.makeJSON() }
    return JSON(updatedWorlds)
}

// Test type 6: Plaintext

drop.get("plaintext") { req in
    return "Hello, World!"
}

try drop.run()
