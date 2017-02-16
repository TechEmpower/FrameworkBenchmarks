import Vapor

final class Fortune: Model {

  static let entity = "fortune"

  var mongoId: Node?
  var id: Node?
  var message: String

  static var idKey = "_id"

  // For internal Vapor use
  var exists: Bool = false

  init(id: Int, message: String) {
    self.id = Node(id)
    self.message = message
  }

  init(node: Node, in context: Context) throws {
    id = try node.extract("_id")
    mongoId = try node.extract("id")
    message = try node.extract("message")
  }

  func makeNode(context: Context) throws -> Node {
    return try Node(node: [
      "id": mongoId,
      "_id": id,
      "message": message
    ])
  }

  func makeJSONNode() throws -> Node {
    return try Node(node: [
      "id": id?.int,
      "message": message
    ])
  }

  func makeJSON() throws -> JSON {
    let node = try makeJSONNode()
    return try JSON(node: node)
  }

  static func prepare(_ database: Database) throws {
    try database.create("Fortune") { fortune in
      fortune.id("id")
      fortune.string("message")
    }
  }

  static func revert(_ database: Database) throws {
    try database.delete("Fortune")
  }

}
