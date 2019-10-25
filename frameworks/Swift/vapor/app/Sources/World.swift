import Fluent
import Vapor

final class World: Model, Content {
    static let schema = "World"
    
    @ID(key: "id")
    var id: Int32?
    
    @ID(key: "randomNumber")
    var randomNumber: Int
    
    init() { }

    init(id: Int? = nil, randomNumber: Int) {
        self.id = id
        self.randomNumber = randomNumber
    }
}
