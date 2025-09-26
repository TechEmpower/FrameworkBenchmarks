import MongoDBVapor
import MongoSwift
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)

let app = Application(env)

defer {
    // Cleanup the application's MongoDB data.
    app.mongoDB.cleanup()
    // Clean up the driver's global state. The driver will no longer be usable from this program after this method is
    // called.
    cleanupMongoSwift()

    app.shutdown()
}

ContentConfiguration.global.use(encoder: ExtendedJSONEncoder(), for: .json)
ContentConfiguration.global.use(decoder: ExtendedJSONDecoder(), for: .json)

app.http.server.configuration.serverName = "Vapor"

app.logger.notice("💧 VAPOR mongo")
app.logger.notice("System.coreCount: \(System.coreCount)")
app.logger.notice("System.maxConnectionsPerEventLoop: \(System.maxConnectionsPerEventLoop)")

let options: MongoClientOptions = MongoClientOptions(maxPoolSize: 56, threadPoolSize: System.maxConnectionsPerEventLoop)

try app.mongoDB.configure("mongodb://tfb-database:27017", options: options)

extension Request {
    var worldCollection: MongoCollection<World> {
        self.mongoDB.client.db("hello_world").collection("world", withType: World.self)
    }
}

app.get("db") { req -> EventLoopFuture<World> in
    let queryId = Int32.random(in: 1...10_000)
    let query: BSONDocument = ["id": .double(Double(queryId))]

    return req.worldCollection.findOne(query)
        .unwrap(or: Abort(.notFound))
}

app.get("queries") { req -> EventLoopFuture<[World]> in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)
    return (0 ..< queries).map { _ -> EventLoopFuture<World> in
        let queryId = Int32.random(in: 1...10_000)
        let query: BSONDocument = ["id": .double(Double(queryId))]

        return req.worldCollection.findOne(query)
            .unwrap(or: Abort(.notFound))
    }.flatten(on: req.eventLoop)
}

extension Int: Sequence {
    public func makeIterator() -> CountableRange<Int>.Iterator {
        return (0..<self).makeIterator()
    }
}

try app.run()
