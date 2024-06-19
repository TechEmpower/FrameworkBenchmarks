import Vapor

struct Fortune: Content {
    var id: Int32?
    var message: String
}
