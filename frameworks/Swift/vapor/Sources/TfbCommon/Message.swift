import Node

public struct Message {
    public let message: String

    public init(_ message: String) {
        self.message = message
    }
}

extension Message: NodeConvertible {
    public init(node: Node) throws {
        message = try node.get("message")
    }

    public func makeNode(in context: Context?) throws -> Node {
        return try Node(node: [
            "message": message
            ])
    }
}
