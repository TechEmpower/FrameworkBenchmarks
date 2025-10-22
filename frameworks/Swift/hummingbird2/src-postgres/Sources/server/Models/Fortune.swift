import Hummingbird
import Mustache

struct Fortune: ResponseEncodable, Sendable {
    var id: Int32
    var message: String
}

// avoid using Mirror as it is expensive
extension Fortune: MustacheParent {
    func child(named: String) -> Any? {
        switch named {
        case "id":
            return id
        case "message":
            return message
        default:
            return nil
        }
    }
}
