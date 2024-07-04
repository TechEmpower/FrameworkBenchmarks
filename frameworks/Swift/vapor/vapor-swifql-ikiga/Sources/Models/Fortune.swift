import Bridges
import SwifQL

final class Fortune: Table, Schemable {
    @Column("id")
    var id: Int32?
    
    @Column("message")
    var message: String
    
    init() {}
    init(id: Int32, message: String) {
        self.id = id
        self.message = message
    }
}

