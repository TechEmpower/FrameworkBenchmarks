import Hummingbird
import HummingbirdFoundation
import PostgresKit

extension Int {
    func bound(_ minValue: Int, _ maxValue: Int) -> Int {
        return Swift.min(maxValue, Swift.max(minValue, self))
    }
}

func runApp() throws {
    let env = HBEnvironment()
    let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
    let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080

    let configuration = HBApplication.Configuration(
        address: .hostname(serverHostName, port: serverPort),
        serverName: "Hummingbird"
    )
    let app = HBApplication(configuration: configuration)
    app.encoder = JSONEncoder()
    app.initConnectionPool()

    WorldController().add(to: app.router)
    FortunesController().add(to: app.router)

    try app.start()
    app.wait()
}

try runApp()
