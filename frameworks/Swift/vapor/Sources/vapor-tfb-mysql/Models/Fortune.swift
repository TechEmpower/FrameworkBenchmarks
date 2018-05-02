import Vapor
import FluentMySQL

final class Fortune: MySQLModel, Migration {
    static let entity = "fortune"

    var id: Int?
    var message: String

    init(id: Int?, message: String) {
        self.id = id
        self.message = message
    }

}
