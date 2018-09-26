import PostgreSQL
import Vapor

struct World: Codable, Content, SQLTable {
    static let sqlTableIdentifierString = "World"
    var id: Int?
    var randomnumber: Int

    init(id: Int? = nil, randomNumber: Int) {
        self.id = id
        self.randomnumber = randomNumber
    }
}
