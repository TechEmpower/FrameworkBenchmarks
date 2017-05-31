import Vapor
import FluentProvider

final class Fortune: Model {
    static let entity = "fortune"

    var storage: Storage = Storage()
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
        id = try node.get("_id")
        mongoId = try node.get("id")
        message = try node.get("message")
    }

    /// Initializes the Fortune from the
    /// database row
    init(row: Row) throws {
        mongoId = try row.get("id")
        id = try row.get("_id")
        message  = try row.get("message")
    }

    // Serializes the Fortune to the database
    func makeRow() throws -> Row {
        var row = Row()

        try row.set("id", mongoId!)
        try row.set("_id", id!)
        try row.set("message", message)

        return row
    }

    func makeNode(context: Context) throws -> Node {
        return try Node(node: [
            "id": mongoId!,
            "_id": id!,
            "message": message
            ])
    }

    func makeJSONNode() throws -> Node {
        return try Node(node: [
            "id": id!.int!,
            "message": message
            ])
    }

    func makeJSON() throws -> JSON {
        let node = try makeJSONNode()
        return JSON(node: node)
    }
}



// MARK: Fluent Preparation

extension Fortune: Preparation {
    /// Prepares a table/collection in the database
    /// for storing Fortunes
    static func prepare(_ database: Database) throws {

    }

    /// Undoes what was done in `prepare`
    static func revert(_ database: Database) throws {
    }
}
