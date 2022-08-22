import Hummingbird
import HummingbirdFoundation
import PostgresNIO

// tfb-server (aka, citrine) uses 28 hyper-threaded cores
// postgresql.conf specifies max_connections = 2000
//
// 2000 / (28 * 2) = 35.7 (theoretical max)
//
// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Environment#citrine-self-hosted
// https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/toolset/databases/postgres/postgresql.conf#L64
let maxConnectionsPerEventLoop = 32

extension Int {
    func bound(_ minValue: Int, _ maxValue: Int) -> Int {
        return Swift.min(maxValue, Swift.max(minValue, self))
    }
}

extension HBApplication {
    var postgresConnectionGroup: HBConnectionPoolGroup<PostgresConnectionSource> {
        get { self.extensions.get(\.postgresConnectionGroup) }
        set { 
            self.extensions.set(\.postgresConnectionGroup, value: newValue) { group in
                try group.close().wait()
            }
        }
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
 
    app.postgresConnectionGroup = .init(
        source: .init(
            configuration: .init(
                connection: .init(host: "tfb-database"),
                authentication: .init(username: "benchmarkdbuser", database: "hello_world", password: "benchmarkdbpass"),
                tls: .disable
            )
        ), 
        maxConnections: maxConnectionsPerEventLoop, 
        eventLoopGroup: app.eventLoopGroup, 
        logger: app.logger
    )
 
    WorldController(connectionPoolGroup: app.postgresConnectionGroup).add(to: app.router)
    FortunesController(connectionPoolGroup: app.postgresConnectionGroup).add(to: app.router)

    try app.start()
    app.wait()
}

try runApp()
