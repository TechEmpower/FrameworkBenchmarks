import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)
let app = Application(environment: env)

app.get("plaintext") { req in
    "Hello, world!"
}

app.get("json") { req in
    ["message": "Hello, world!"]
}

try app.run()
