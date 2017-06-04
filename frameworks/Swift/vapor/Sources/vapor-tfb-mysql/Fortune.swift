import Vapor
import FluentProvider

final class Fortune: Model {
    
    static let entity = "Fortune"
    
    var id: Node?
    
    var message: String
    var storage: Storage = Storage()
    
    // For internal Vapor use
    var exists: Bool = false
    
    init(id: Int, message: String) {
        self.id = Node(id)
        self.message = message
    }
    
    init(node: Node, in context: Context) throws {
        id = try node.get("id")
        message = try node.get("message")
    }
    
    init(row: Row) throws {
        self.id = try row.get("id")
        message = try row.get("message")
    }
    
    func makeRow() throws -> Row {
        var row = Row()
        try row.set("id", id)
        try row.set("message", message)
        
        return row
    }
    
    func makeNode(context: Context) throws -> Node {
        return try Node(node: [
            "id": id,
            "message": message
            ])
    }
    
    static func prepare(_ database: Database) throws {
        try database.create(self) { fortune in
            fortune.id()
            fortune.string("message")
        }
    }
    
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
    
}

extension Fortune: JSONRepresentable {
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set("id", id)
        try json.set("message", message)
        return json
    }
}
