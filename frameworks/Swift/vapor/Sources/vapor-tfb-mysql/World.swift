import Vapor
import FluentProvider

final class World: Model {
    
    var storage: Storage = Storage()
    
    static let entity = "World"
    
    var id: Node?
    var randomNumber: Int32
    
    // For internal Vapor use
    var exists: Bool = false
    
    init(node: Node, in context: Context) throws {
        id = try node.get("id")
        randomNumber = try node.get("randomnumber")
    }
    
    init(row: Row) throws {
        id = try row.get("id")
        randomNumber = try row.get("randomnumber")
    }
    
    func makeRow() throws -> Row {
        var row = Row()
        try row.set("id", id)
        try row.set("randomNumber", randomNumber)
        
        return row
    }
    
    func makeNode(context: Context) throws -> Node {
        return try Node(node: [
            "id": id,
            "randomNumber": randomNumber
            ])
    }
    
    static func prepare(_ database: Database) throws {
        try database.create(self, closure: { (world) in
            world.id()
            world.int("randomNumber")
        })
        
    }
    
    static func revert(_ database: Database) throws {
        try database.delete(self)
    }
}

extension World: JSONRepresentable {
    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set("id", id)
        try json.set("randomNumber", randomNumber)
        return json
    }
}
