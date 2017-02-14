import Vapor

final class World: Model {
  
  static let entity = "World"
  
  var id: Node?
  var randomNumber: Int32
  
  // For internal Vapor use
  var exists: Bool = false
  
  init(node: Node, in context: Context) throws {
    id = try node.extract("id")
    randomNumber = try node.extract("randomNumber")
  }
  
  func makeNode(context: Context) throws -> Node {
    return try Node(node: [
      "id": id,
      "randomNumber": randomNumber
    ])
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
