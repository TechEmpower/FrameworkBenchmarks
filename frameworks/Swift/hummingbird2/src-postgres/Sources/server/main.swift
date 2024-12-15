import Foundation
import Hummingbird
import PostgresNIO

// postgresql.conf specifies max_connections = 2000
// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Environment#citrine-self-hosted
// https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/toolset/databases/postgres/postgresql.conf#L64

extension Int {
    func bound(_ minValue: Int, _ maxValue: Int) -> Int {
        return Swift.min(maxValue, Swift.max(minValue, self))
    }
}

struct TechFrameworkRequestContext: RequestContext {
    static let jsonEncoder = JSONEncoder()
    static let jsonDecoder = JSONDecoder()

    var coreContext: Hummingbird.CoreRequestContextStorage

    // Use a global JSON Encoder
    var responseEncoder: JSONEncoder { Self.jsonEncoder }
    // Use a global JSON Decoder
    var requestDecoder: JSONDecoder { Self.jsonDecoder }

    init(source: ApplicationRequestContextSource) {
        self.init(channel: source.channel, logger: source.logger)
    }

    init(channel: any Channel, logger: Logger) {
        self.coreContext = CoreRequestContextStorage(
            source: ApplicationRequestContextSource(channel: channel, logger: logger)
        )
    }
}

func runApp() async throws {
    let env = Environment()
    let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
    let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080

    var postgresConfiguration = PostgresClient.Configuration(
        host: "tfb-database", 
        username: "benchmarkdbuser", 
        password: "benchmarkdbpass", 
        database: "hello_world", 
        tls: .disable
    )
    postgresConfiguration.options.maximumConnections = 100
    let postgresClient = PostgresClient(
        configuration: postgresConfiguration, 
        eventLoopGroup: MultiThreadedEventLoopGroup.singleton
    )
    let router = Router(context: TechFrameworkRequestContext.self)
    router.addRoutes(WorldController(postgresClient: postgresClient).routes)
    router.addRoutes(FortunesController(postgresClient: postgresClient).routes)
    var app = Application(
        router: router,
        configuration: .init(
            address: .hostname(serverHostName, port: serverPort),
            serverName: "HB2",
            backlog: 8192
        )
    )
    app.addServices(postgresClient)
    try await app.runService()
}

try await runApp()
