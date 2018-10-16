import Vapor

/// Register your application's routes here.
public func routes(_ router: Router) throws {

    let jsonController = JSONController()
    router.get("json", use: jsonController.get)

    let plainTextController = PlainTextController()
    router.get("plaintext", use: plainTextController.get)

    let worldController = WorldController()
    router.get("db", use: worldController.get)
    router.get("queries", use: worldController.queries)
}
