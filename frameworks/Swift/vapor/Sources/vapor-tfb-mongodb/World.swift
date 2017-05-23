import Vapor
import FluentProvider

final class World: Model {
    
    static let entity = "world"
    
    var storage: Storage = Storage()
    var mongoId: Node?
    var id: Node?
    var randomNumber: Int32
    
    static var idKey = "_id"
    
    // For internal Vapor use
    var exists: Bool = false
    
    init(node: Node, in context: Context) throws {
        mongoId = try node.get("id")
        id = try node.get("_id")
        randomNumber = try node.get("randomNumber")
    }
    
    init(row: Row) throws {
        mongoId = try row.get("id")
        id = try row.get("_id")
        randomNumber = try row.get("randomNumber")
    }
    
    func makeRow() throws -> Row {
        var row = Row()
        
        try row.set("id", mongoId)
        try row.set("_id", id)
        try row.set("randomNumber", randomNumber)
        
        return row
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
            "id": mongoId?.int,
            "_id": id?.int,
            "randomNumber": randomNumber
            ])
    }
    
    func makeJSON() throws -> JSON {
        let node = try makeJSONNode()
        return try JSON(node: node)
    }
    
    static func prepare(_ database: Database) throws {
        try database.create(self) { world in
            world.id()
            world.int("randomNumber")
        }
    }
    
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}
