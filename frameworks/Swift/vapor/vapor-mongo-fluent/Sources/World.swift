import Fluent
import Vapor

final class World: Model, Content {
    static let schema = "world"

    @ID(custom: "id")
    var id: Float?

    @Field(key: "randomNumber")
    var randomNumber: Float

    init() { }
}
