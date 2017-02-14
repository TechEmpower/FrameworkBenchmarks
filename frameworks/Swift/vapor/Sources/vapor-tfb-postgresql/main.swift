import Vapor
import JSON
import HTTP
import VaporPostgreSQL

import TfbCommon

let drop = Droplet()
try drop.addProvider(VaporPostgreSQL.Provider.self)

// All test types require `Server` and `Date` HTTP response headers.
// Vapor has standard middleware that adds `Date` header.
// We use custom middleware that adds `Server` header.
drop.middleware.append(ServerMiddleware())

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
drop.get("fortunes") { _ in
  let fortunes = try Fortune.all().flatMap { try $0.makeNode() }
  return try drop.view.make("fortune", ["fortunes": Node(fortunes)])
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
let helloWorldBuffer = "Hello, World!".utf8.array
drop.get("plaintext") { req in
  return Response(headers: ["Content-Type": "text/plain; charset=utf-8"], body: helloWorldBuffer)
}

drop.run()
