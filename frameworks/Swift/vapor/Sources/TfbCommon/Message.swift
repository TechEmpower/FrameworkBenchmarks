import Node

public struct Message {
  public let message: String
  
  public init(_ message: String) {
    self.message = message
  }
}

extension Message: NodeConvertible {
  public init(node: Node, in context: Context) throws {
    message = try node.extract("message")
  }
  
  public func makeNode(context: Context) throws -> Node {
    return try Node(node: [
      "message": message
      ])
  }
}
