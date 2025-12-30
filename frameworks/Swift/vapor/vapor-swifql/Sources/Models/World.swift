import Bridges

final class World: Table {
    @Column("id")
    var id: Int32?
    
    @Column("randomnumber")
    var randomnumber: Int
    
    init() {}
}
