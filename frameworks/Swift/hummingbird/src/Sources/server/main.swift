import Hummingbird
import HummingbirdFoundation

struct Object: HBResponseEncodable {
    let message: String
}

func runApp() throws {
    let env = HBEnvironment()
    let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
    let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080

    let configuration = HBApplication.Configuration(
        address: .hostname(serverHostName, port: serverPort),
        serverName: "Hummingbird",
        enableHttpPipelining: false
    )
    let app = HBApplication(configuration: configuration)
    app.encoder = JSONEncoder()

    app.router.get("plaintext") { req in
        "Hello, world!"
    }

    app.router.get("json") { req in
        Object(message: "Hello, world!")
    }

    try app.start()
    app.wait()
}

try runApp()
