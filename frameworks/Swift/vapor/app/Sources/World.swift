import Fluent
import Vapor

final class World: Model, Content {
    static let schema = "World"
    
    @ID(key: "id")
    var id: Int32?
    
    @Field(key: "randomnumber")
    var randomnumber: Int
    
    init() { }
}
