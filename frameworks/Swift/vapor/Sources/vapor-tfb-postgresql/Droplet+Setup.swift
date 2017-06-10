@_exported import Vapor

extension Droplet {
    public func setup() throws {
        try collection(Routes.self)

        Fortune.database = try drop.assertDatabase()
        World.database = try drop.assertDatabase()
    }
}
