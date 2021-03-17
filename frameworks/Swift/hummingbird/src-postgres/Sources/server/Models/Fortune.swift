import Hummingbird

struct Fortune: HBResponseEncodable {
    var id: Int32?
    var message: String
}

