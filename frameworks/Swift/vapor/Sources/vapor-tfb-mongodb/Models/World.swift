import Vapor
import FluentProvider

final class World: Model {
    static let entity = "world"
    
    let storage = Storage()

    /// The content of the world
    var mongoId: Node?
    var id: Node?
    var randomNumber: Int32

    /// Creates a new World
    init(_id: Node, mongoId: Node, randomNumber: Int32) {
        self.id = _id
        self.mongoId = mongoId
        self.randomNumber = randomNumber
    }

    // MARK: Fluent Serialization

    /// Initializes the World from the
    /// database row
    init(row: Row) throws {
        mongoId = try row.get("id")
        id = try row.get("_id")
        randomNumber  = try row.get("randomNumber")
    }

    // Serializes the World to the database
    func makeRow() throws -> Row {
        var row = Row()

        try row.set("id", mongoId)
//        try row.set("_id", id)
        try row.set("randomNumber", randomNumber)

        return row
    }
}

// MARK: Fluent Preparation

extension World: Preparation {
    /// Prepares a table/collection in the database
    /// for storing Worlds
    static func prepare(_ database: Database) throws {
    }

    /// Undoes what was done in `prepare`
    static func revert(_ database: Database) throws {
    }
}

// MARK: JSON
extension World: JSONConvertible {
    convenience init(json: JSON) throws {
        try self.init(
            _id: json.get("_id"),
            mongoId: json.get("id"),
            randomNumber: json.get("randomNumber")
        )
    }

    func makeJSON() throws -> JSON {
        var json = JSON()
//        try json.set("_id", id)
        try json.set("id", mongoId)
        try json.set("randomNumber", randomNumber)
        return json
    }
}

// MARK: HTTP

extension World: ResponseRepresentable { }
