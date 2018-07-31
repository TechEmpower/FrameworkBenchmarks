import FluentMySQL
import Leaf
import Vapor

// Services
var services = Services.default()

// Router
let router = EngineRouter.default()

// Config
var config = Config.default()

// Routes

var jsonRes = HTTPResponse(status: .ok, headers: ["Server": "Vapor"])
let jsonEncoder = JSONEncoder()

// Test type 2: Single database query
router.get("db") { req -> Future<Response> in
    let worldId = WorldMeta.randomId()
    return try World.find(worldId, on: req)
        .map(to: Response.self) { world in
            let res = Response(http: jsonRes, using: req.sharedContainer)

            if let world = world {
                try res.content.encode(json: world, using: jsonEncoder)
            } else {
                try res.content.encode(json: Message(), using: jsonEncoder)
            }

            return res
    }
}

// Test type 3: Multiple database queries
router.get("queries", String.parameter) { req -> Future<Response> in
    let queries = try queriesParam(for: req)
    let ids = (1...queries).lazy.map({ _ in WorldMeta.randomId() })

    return try ids.map { id in
            return try World.find(id, on: req)
        }
        .flatten(on: req)
        .map { result in
            return result.compactMap({ $0 })
        }.map { worlds in
            let res = Response(http: jsonRes, using: req.sharedContainer)
            try res.content.encode(json: worlds, using: jsonEncoder)
            return res
        }
}

// Test type 4: Fortunes
router.get("fortunes") { req -> Future<View> in
    let posixLocale = Locale(identifier: "en_US_POSIX")

    return Fortune.query(on: req).all()
        .map { fortunes -> [Fortune] in
            var newFortunes = fortunes
            let additional = Fortune(id: 0, message: "Additional fortune added at request time.")
            newFortunes.insert(additional, at: 0)
            newFortunes.sort(by: { lhs, rhs -> Bool in
                return lhs.message.compare(rhs.message, locale: posixLocale) == .orderedAscending
            })
            return newFortunes
        }
        .flatMap { fortunes in
            // FIXME: Add headers
            return try req.view().render("fortune", ["fortunes": fortunes])
        }
}

// Test type 5: Database updates
router.get("updates", String.parameter) { req -> Future<Response> in
    let queries = try queriesParam(for: req)
    let ids = (1...queries).lazy.map({ _ in WorldMeta.randomId() })

    return try ids.map { id in
            return try World.find(id, on: req)
        }
        .flatten(on: req)
        .map { result -> [World] in
            let worlds = result.compactMap({ $0 })
            worlds.forEach { $0.randomNumber = WorldMeta.randomRandomNumber() }
            return worlds
        }
        .flatMap { worlds -> Future<[World]> in
            return worlds
                .map { $0.save(on: req) }
                .flatten(on: req)
        }.flatMap { _ in
            return World.query(on: req).all()
        }.map { worlds in
            let res = Response(http: jsonRes, using: req.sharedContainer)
            try res.content.encode(json: worlds, using: jsonEncoder)
            return res 
        }
}

services.register(router, as: Router.self)

try services.register(LeafProvider())
config.prefer(LeafRenderer.self, for: ViewRenderer.self)
try services.register(FluentMySQLProvider())
/// Register custom MySQL Config
let mySQLUrl = URL(string: "mysql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world")!
let mysqlConfig = MySQLDatabaseConfig(hostname: mySQLUrl.host!, port: mySQLUrl.port!, username: mySQLUrl.user!, password: mySQLUrl.password, database: "hello_world")
services.register(mysqlConfig)

/// Configure migrations
services.register(MigrationConfig())
/// Allow requests as DatabaseConnectible
Fortune.defaultDatabase = .mysql
World.defaultDatabase = .mysql

// Middlewares (remove unused ErrorMiddleware)
var middlewares = MiddlewareConfig()
services.register(middlewares)

let app = try Application(config: config, environment: .detect(), services: services)
try app.run()
