import Vapor

// Models
struct Message: Content {
	let message: String?
}

// Services
var services = Services.default()
// services.register(ServerMiddleware.self)

// Routes
let router = EngineRouter.default()
router.get("json") { req in
    return Message(message: nil)
}
router.get("plaintext") { req in
    return "Hello, world!" as StaticString
}
services.register(router, as: Router.self)

// Middlewares
var middlewares = MiddlewareConfig()
//middlewares.use(ServerMiddleware.self)
services.register(middlewares)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()
