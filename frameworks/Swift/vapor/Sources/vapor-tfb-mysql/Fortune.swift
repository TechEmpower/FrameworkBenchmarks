import Vapor
import FluentProvider

final class Fortune: Model {

  static let entity = "Fortune"

  var id: Node?

  var message: String

  // For internal Vapor use
  var exists: Bool = false

  init(id: Int, message: String) {
    self.id = Node(id)
    self.message = message
  }

  init(node: Node, in context: Context) throws {
    id = try node.extract("id")
    message = try node.extract("message")
  }

  func makeNode(context: Context) throws -> Node {
    return try Node(node: [
      "id": id,
      "message": message
    ])
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
