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
    
    init(row: Row) throws {
        mongoId = try row.get("id")
        id = try row.get("_id")
        message = try row.get("message")
    }
    
    func makeRow() throws -> Row {
        var row = Row()
        
        try row.set("id", mongoId)
        try row.set("_id", id)
        try row.set("message", message)
        
        return row
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
            "_id": id?.int,
            "id": mongoId,
            "message": message
            ])
    }
    
    func makeJSON() throws -> JSON {
        let node = try makeJSONNode()
        return try JSON(node: node)
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
