import Hummingbird
import PostgresKit

class WorldController {
    func add(to router: HBRouter) {
        router.get("db", use: single)
        router.get("queries", use: multiple)
        router.get("updates", use: updates)
    }

    func single(request: HBRequest) -> EventLoopFuture<World> {
        request.db.query("SELECT id, randomnumber FROM World WHERE id = $1", [
            PostgresData(int32: .random(in: 1...10_000))
        ]).flatMapThrowing { result -> World in
            guard let firstResult = result.first else { throw HBHTTPError(.notFound) }
            return World(
                id: firstResult.column("id")?.int32 ?? 0,
                randomNumber: firstResult.column("randomnumber")?.int ?? 0
            )
        }
    }

    func multiple(request: HBRequest) -> EventLoopFuture<[World]> {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        let futures: [EventLoopFuture<World>] = (0 ..< queries).map { _ -> EventLoopFuture<World> in
            request.db.query("SELECT id, randomnumber FROM World WHERE id = $1", [
                PostgresData(int32: .random(in: 1...10_000))
            ]).flatMapThrowing { result -> World in
                guard let firstResult = result.first else { throw HBHTTPError(.notFound) }
                return World(
                    id: firstResult.column("id")?.int32 ?? 0,
                    randomNumber: firstResult.column("randomnumber")?.int ?? 0
                )
            }
        }
        return EventLoopFuture.whenAllSucceed(futures, on: request.eventLoop)
    }

    func updates(request: HBRequest) -> EventLoopFuture<[World]> {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        let ids = (0 ..< queries).map { _ in Int32.random(in: 1...10_000) }
        let futures: [EventLoopFuture<World>] = ids.map { _ -> EventLoopFuture<World> in
            request.db.query("SELECT id, randomnumber FROM World WHERE id = $1", [
                PostgresData(int32: .random(in: 1...10_000))
            ]).flatMap { result in
                guard let firstResult = result.first else { return request.failure(.notFound) }
                let id = firstResult.column("id")?.int32 ?? 0
                let randomNumber = Int32.random(in: 1...10_000)
                return request.db.query("UPDATE World SET randomnumber = $1 WHERE id = $2", [
                    PostgresData(int32: randomNumber),
                    PostgresData(int32: id)
                ]).map { ($0, World(id: id, randomNumber: numericCast(randomNumber))) }
            }.map { (result: PostgresQueryResult, world) in
                return world
            }
        }
        return EventLoopFuture.whenAllSucceed(futures, on: request.eventLoop)
    }
}
