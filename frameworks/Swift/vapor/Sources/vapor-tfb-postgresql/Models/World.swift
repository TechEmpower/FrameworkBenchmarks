import Vapor
import FluentProvider

final class World: Model {
    static let entity = "world"
    
    let storage = Storage()

    /// The content of the post
    var id: Node
    var randomNumber: Int32

    /// Creates a new World
    init(id: Node, randomNumber: Int32) {
        self.id = id
        self.randomNumber = randomNumber
    }

    // MARK: Fluent Serialization

    /// Initializes the World from the
    /// database row
    init(row: Row) throws {
        id = try row.get("id")
        randomNumber  = try row.get("randomnumber")
    }

    // Serializes the Post to the database
    func makeRow() throws -> Row {
        var row = Row()

        try row.set("id", id)
        try row.set("randomnumber", randomNumber)

        return row
    }
}

// MARK: Fluent Preparation

extension World: Preparation {
    /// Prepares a table/collection in the database
    /// for storing Posts
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
            id: json.get("id"),
            randomNumber: json.get("randomnumber")
        )
    }

    func makeJSON() throws -> JSON {
        var json = JSON()
        try json.set("id", id)
        try json.set("randomnumber", randomNumber)
        return json
    }
}

// MARK: HTTP

// This allows Post models to be returned
// directly in route closures
extension World: ResponseRepresentable { }
