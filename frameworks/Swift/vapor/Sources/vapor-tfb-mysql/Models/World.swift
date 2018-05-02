import Vapor
import FluentMySQL

final class World: MySQLModel, Migration, Content {
    static let entity = "world"
    
    var id: Int?
    var randomNumber: Int

    /// Creates a new World
    init(id: Int?, randomNumber: Int) {
        self.id = id
        self.randomNumber = randomNumber
    }
}
