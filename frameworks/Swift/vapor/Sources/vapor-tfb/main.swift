import Vapor

// Services
var services = Services.default()

// Router
let router = EngineRouter.default()

// Routes

// JSON test
var jsonRes = HTTPResponse(status: .ok, headers: ["Server": "Vapor"])
let jsonEncoder = JSONEncoder()

router.get("json") { req -> Response in
    let res = Response(http: jsonRes, using: req.sharedContainer)
    try res.content.encode(json: Message(message: "Hello, world!"), using: jsonEncoder)
    return res
}

// Plaintext test
let plaintextRes = HTTPResponse(status: .ok, headers: ["Server": "Vapor", "Content-Type": "text/plain"], body: "Hello, world!")

router.get("plaintext") { req in
    return plaintextRes
}

services.register(router, as: Router.self)

// Middlewares (remove unused ErrorMiddleware)
var middlewares = MiddlewareConfig()
services.register(middlewares)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()
