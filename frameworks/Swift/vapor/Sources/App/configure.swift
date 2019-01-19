import PostgreSQL
import Vapor

class ServerHeaderMiddleWare : Middleware, Service {
    func respond(to request: Request, chainingTo next: Responder) throws -> EventLoopFuture<Response> {
        return try next.respond(to: request).map { response in
            response.http.headers.add(name: HTTPHeaderName.server, value: "Vapor")
            return response
        }
    }
}

public func configure(_ config: inout Config, _ env: inout Environment, _ services: inout Services) throws {
    let router = EngineRouter.default()
    try routes(router)
    services.register(router, as: Router.self)
    services.register(ServerHeaderMiddleWare.self) { _ in
        return ServerHeaderMiddleWare()
    }

    var middlewares = MiddlewareConfig()
    middlewares.use(ErrorMiddleware.self)
    middlewares.use(ServerHeaderMiddleWare.self)
    services.register(middlewares)

    try services.register(PostgreSQLProvider())

    let postgresql = PostgreSQLDatabase(
        config: PostgreSQLDatabaseConfig(
            hostname: "tfb-database",
            username: "benchmarkdbuser",
            database: "hello_world",
            password: "benchmarkdbpass"
    ))

    var databases = DatabasesConfig()
    databases.add(database: postgresql, as: .psql)
    services.register(databases)
}
