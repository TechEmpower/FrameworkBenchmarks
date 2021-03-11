import Hummingbird
import HummingbirdFoundation
import PostgresKit

struct World: HBResponseEncodable {
    var id: Int32?
    var randomnumber: Int
}

extension Int {
    func bound(_ minValue: Int, _ maxValue: Int) -> Int {
        return Swift.min(maxValue, Swift.max(minValue, self))
    }
}

func runApp() {
    let env = HBEnvironment()
    let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
    let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080

    let configuration = HBApplication.Configuration(
        address: .hostname(serverHostName, port: serverPort),
        serverName: "Hummingbird"
    )
    let app = HBApplication(configuration: configuration)
    app.encoder = JSONEncoder()
    app.middleware.add(HBDateResponseMiddleware(application: app))
    app.initConnectionPool()
    
    app.router.get("db") { request in
        request.db.query("SELECT id, randomnumber FROM World WHERE id = $1", [
            PostgresData(int32: .random(in: 1...10_000))
        ]).flatMapThrowing { result -> World in
            guard let firstResult = result.first else { throw HBHTTPError(.notFound) }
            return World(
                id: firstResult.column("id")?.int32 ?? 0,
                randomnumber: firstResult.column("randomnumber")?.int ?? 0
            )
        }
    }
    
    app.router.get("queries") { request -> EventLoopFuture<[World]> in
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        let futures: [EventLoopFuture<World>] = (0 ..< queries).map { _ -> EventLoopFuture<World> in
            request.db.query("SELECT id, randomnumber FROM World WHERE id = $1", [
                PostgresData(int32: .random(in: 1...10_000))
            ]).flatMapThrowing { result -> World in
                guard let firstResult = result.first else { throw HBHTTPError(.notFound) }
                return World(
                    id: firstResult.column("id")?.int32 ?? 0,
                    randomnumber: firstResult.column("randomnumber")?.int ?? 0
                )
            }
        }
        return EventLoopFuture.whenAllSucceed(futures, on: request.eventLoop)
    }

    app.start()
    app.wait()
}

runApp()
