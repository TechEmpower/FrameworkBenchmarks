import Bridges

final class Fortune: Table {
    @Column("id")
    var id: Int32?
    
    @Column("message")
    var message: String
    
    init() {}
}

