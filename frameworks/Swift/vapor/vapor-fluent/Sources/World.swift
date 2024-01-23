import Fluent
import Vapor

final class World: Model, Content {
    static let schema = "World"
    
    @ID(custom: "id")
    var id: Int32?
    
    @Field(key: "randomnumber")
    var randomnumber: Int
    
    init() { }
}
