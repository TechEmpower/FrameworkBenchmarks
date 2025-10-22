import Foundation
import Hummingbird
import HummingbirdCore
import Logging
import NIOCore

struct Object: ResponseEncodable {
    let message: String
}

struct TechFrameworkRequestContext: RequestContext {
    var coreContext: CoreRequestContextStorage

    init(source: ApplicationRequestContextSource) {
        self.coreContext = CoreRequestContextStorage(source: source)
    }
}

func runApp() async throws {
    let env = Environment()
    let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
    let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080

    let router = Router(context: TechFrameworkRequestContext.self)
    router.get("plaintext") { _, _ in
        "Hello, world!"
    }
    router.get("json") { _, _ in
        Object(message: "Hello, world!")
    }
    let app = Application(
        router: router,
        configuration: .init(
            address: .hostname(serverHostName, port: serverPort),
            serverName: "HB",
            backlog: 8192
        )
    )
    try await app.runService()
}

try await runApp()
