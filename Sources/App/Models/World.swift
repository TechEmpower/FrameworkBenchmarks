import Bridges

final class World: Table {
    @Column("id")
    var id: Int?
    
    @Column("randomnumber")
    var randomnumber: Int
    
    init() {}
}
