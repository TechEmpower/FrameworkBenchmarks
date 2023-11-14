import Hummingbird
import PostgresNIO

struct WorldController {
    let connectionPoolGroup: HBConnectionPoolGroup<PostgresConnectionSource>

    func add(to router: HBRouterBuilder) {
        router.get("db", use: single)
        router.get("queries", use: multiple)
        router.get("updates", use: updates)
    }

    func single(request: HBRequest) -> EventLoopFuture<World> {
        let id = Int32.random(in: 1...10_000)
        return self.connection(for: request) { connection in
            return connection.query("SELECT id, randomnumber FROM World WHERE id = $1", [
                PostgresData(int32: id)
            ])
        }.flatMapThrowing { result -> World in
            guard let firstResult = result.first else { throw HBHTTPError(.notFound) }
            let result = try firstResult.decode((Int32, Int32).self, context: .default)
            return World(id: result.0, randomNumber: result.1)
        }
    }

    func multiple(request: HBRequest) -> EventLoopFuture<[World]> {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        let futures: [EventLoopFuture<World>] = (0 ..< queries).map { _ -> EventLoopFuture<World> in
            let id = Int32.random(in: 1...10_000)
            return self.connection(for: request) { connection in
                return connection.query("SELECT id, randomnumber FROM World WHERE id = $1", [
                    PostgresData(int32: id)
                ])
            }.flatMapThrowing { result -> World in
                guard let firstResult = result.first else { throw HBHTTPError(.notFound) }
                let result = try firstResult.decode((Int32, Int32).self, context: .default)
                return World(id: result.0, randomNumber: result.1)
            }
        }
        return EventLoopFuture.whenAllSucceed(futures, on: request.eventLoop)
    }

    func updates(request: HBRequest) -> EventLoopFuture<[World]> {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        let ids = (0 ..< queries).map { _ in Int32.random(in: 1...10_000) }
        let futures: [EventLoopFuture<World>] = ids.map { _ -> EventLoopFuture<World> in
            let id = Int32.random(in: 1...10_000)
            let randomNumber = Int32.random(in: 1...10_000)
            return self.connection(for: request) { connection in
                return connection.query("SELECT id, randomnumber FROM World WHERE id = $1", [
                    PostgresData(int32: id)
                ]).flatMap { result in
                    return connection.query("UPDATE World SET randomnumber = $1 WHERE id = $2", [
                        PostgresData(int32: randomNumber),
                        PostgresData(int32: id)
                    ])
                }
            }.map { _ in
                return World(id: id, randomNumber: randomNumber)
            }
        }
        return EventLoopFuture.whenAllSucceed(futures, on: request.eventLoop)
    }

    @discardableResult func connection<NewValue>(for request: HBRequest, closure: @escaping (PostgresConnection) -> EventLoopFuture<NewValue>) -> EventLoopFuture<NewValue> {
        return self.connectionPoolGroup.lease(on: request.eventLoop, logger: request.logger, process: closure)
    }
}
