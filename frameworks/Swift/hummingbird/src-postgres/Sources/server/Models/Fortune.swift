import Hummingbird
import HummingbirdMustache

struct Fortune: HBResponseEncodable {
    var id: Int32
    var message: String
}

// avoid using Mirror as it is expensive
extension Fortune: HBMustacheParent {
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
