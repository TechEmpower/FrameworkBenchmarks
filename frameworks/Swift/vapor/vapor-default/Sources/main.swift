import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)

let app = Application(env)
defer { app.shutdown() }

app.http.server.configuration.serverName = "Vapor"
app.http.server.configuration.supportPipelining = false

app.logger.notice("💧 VAPOR")
app.logger.notice("System.coreCount: \(System.coreCount)")

app.get("plaintext") { req async in
    "Hello, world!"
}

app.get("json") { req async in
    ["message": "Hello, world!"]
}

try app.run()
