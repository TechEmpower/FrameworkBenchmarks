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

    services.register(EngineServerConfig.default(hostname: "0.0.0.0"))
    try services.register(LeafProvider())
    try services.register(FluentMySQLProvider())
    /// Register custom MySQL Config
    let mySQLUrl = URL(string: "mysql://benchmarkdbuser:benchmarkdbpass@tfb-database:3306/hello_world")!
    let mysqlConfig = MySQLDatabaseConfig(hostname: mySQLUrl.host!, port: mySQLUrl.port!, username: mySQLUrl.user!, password: mySQLUrl.password!, database: "hello_world")
    services.register(mysqlConfig)
    
    /// Configure migrations
    var migrations = MigrationConfig()
    migrations.add(model: Fortune.self, database: .mysql)
    migrations.add(model: World.self, database: .mysql)
    services.register(migrations)
    
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
