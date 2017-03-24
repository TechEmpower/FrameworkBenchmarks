import Vapor

final class World: Model {
  
  static let entity = "world"
  
  var mongoId: Node?
  var id: Node?
  var randomNumber: Int32
  
  static var idKey = "_id"
  
  // For internal Vapor use
  var exists: Bool = false
  
  init(node: Node, in context: Context) throws {
    mongoId = try node.extract("id")
    id = try node.extract("_id")
    randomNumber = try node.extract("randomNumber")
  }
  
  func makeNode(context: Context) throws -> Node {
    return try Node(node: [
      "id": mongoId,
      "_id": id,
      "randomNumber": randomNumber
    ])
  }
  
  func makeJSONNode() throws -> Node {
    return try Node(node: [
      "id": id?.int,
      "randomNumber": randomNumber
    ])
  }
  
  func makeJSON() throws -> JSON {
    let node = try makeJSONNode()
    return try JSON(node: node)
  }
  
  static func prepare(_ database: Database) throws {
    try database.create("World") { world in
      world.id("id")
      world.int("randomNumber")
    }
  }
  
  static func revert(_ database: Database) throws {
    try database.delete("World")
  }
}
