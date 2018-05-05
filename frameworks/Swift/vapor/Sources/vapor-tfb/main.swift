import Vapor

// Services
var services = Services.default()

// Router
let router = EngineRouter.default()
// Routes
let json = Message(message: "Hello, world!")
router.get("json") { req in
    return json.encode(status: .ok, headers: ["Server": "Vapor"], for: req)
}
let plaintextRes = HTTPResponse(status: .ok, headers: ["Server": "Vapor", "Content-Type": "text/plain"], body: "Hello, world!")
router.get("plaintext") { req in
    return plaintextRes
}
services.register(router, as: Router.self)

// Middlewares (remove default mws)
var middlewares = MiddlewareConfig()
services.register(middlewares)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()
