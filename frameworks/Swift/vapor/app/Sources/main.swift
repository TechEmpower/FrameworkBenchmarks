import Fluent
import FluentPostgresDriver
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)
let app = Application(environment: env)

app.provider(FluentProvider())
app.register(Migrations.self) { _ in .init() }

app.register(MiddlewareConfiguration.self) { app in
    var middlewares = MiddlewareConfiguration()
    middlewares.use(ServerMiddleware())
    middlewares.use(ErrorMiddleware.default(environment: app.environment))
    return middlewares
}

app.databases.postgres(
    configuration: .init(
        hostname: "tfb-database",
        username: "benchmarkdbuser",
        password: "benchmarkdbpass",
        database: "hello_world"
    ),
    on: app.make()
)

app.get("plaintext") { req in
    "Hello, world!"
}

app.get("json") { req in
    ["message": "Hello, world!"]
}

app.get("db") { req in
    World.find(.random(in: 1 ... 10_000), on: req.db)
        .unwrap(or: Abort(.notFound))
}

app.get("queries") { req -> EventLoopFuture<[World]> in
    let queries = (req.query["queries"] ?? 1).bounded(to: 1...500)
    return (0 ..< queries).map { _ -> EventLoopFuture<World> in
        World.find(.random(in: 1 ... 10_000), on: req.db)
            .unwrap(or: Abort(.notFound))
    }.flatten(on: req.eventLoop)
}

try app.run()
