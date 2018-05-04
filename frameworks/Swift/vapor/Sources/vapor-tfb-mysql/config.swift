import FluentMySQL
import Leaf
import Vapor
import TfbCommon

/// Called before your application initializes.
///
/// [Learn More →](https://docs.vapor.codes/3.0/getting-started/structure/#configureswift)
public func configure(
    _ config: inout Config,
    _ env: inout Environment,
    _ services: inout Services
) throws {
    services.register(ServerMiddleware.self)
    try services.register(LeafProvider())
    config.prefer(LeafRenderer.self, for: ViewRenderer.self)
    try services.register(FluentMySQLProvider())
    /// Register custom MySQL Config
    let mySQLUrl = URL(string: "mysql://vapor_username:vapor_username@localhost:3306/hello_world")!
    let mysqlConfig = MySQLDatabaseConfig(hostname: mySQLUrl.host!, port: mySQLUrl.port!, username: mySQLUrl.user!, password: mySQLUrl.password, database: "hello_world")
    services.register(mysqlConfig)
    
    
    /// Configure migrations
    services.register(MigrationConfig())
    /// Allow requests as DatabaseConnectible
    Fortune.defaultDatabase = .mysql
    World.defaultDatabase = .mysql
    
    // Register routes to the router
    let router = EngineRouter.default()
    try routes(router)
    services.register(router, as: Router.self)

    // Register middleware
    var middlewares = MiddlewareConfig()
    middlewares.use(ServerMiddleware.self)
    services.register(middlewares)

    // Configure the rest of your application here
}
