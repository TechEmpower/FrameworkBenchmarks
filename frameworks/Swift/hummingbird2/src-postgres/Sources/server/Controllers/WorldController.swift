import Hummingbird
import PostgresNIO

struct WorldController {
    typealias Context = TechFrameworkRequestContext
    let postgresClient: PostgresClient

    var routes: RouteCollection<Context> {
        RouteCollection(context: Context.self)
            .get("db", use: single)
            .get("queries", use: multiple)
            .get("updates", use: updates)
    }

    /// In this test, each request is processed by fetching a single row from a 
    /// simple database table. That row is then serialized as a JSON response.
    @Sendable func single(request: Request, context: Context) async throws -> World {
        let id = Int32.random(in: 1...10_000)
        let rows = try await self.postgresClient.query("SELECT id, randomnumber FROM World WHERE id = \(id)")
        for try await (id, randomNumber) in rows.decode((Int32, Int32).self, context: .default) {
            return World(id: id, randomNumber: randomNumber)
        }
        throw HTTPError(.notFound)
    }

    /// In this test, each request is processed by fetching multiple rows from a 
    /// simple database table and serializing these rows as a JSON response. The 
    /// test is run multiple times: testing 1, 5, 10, 15, and 20 queries per request. 
    /// All tests are run at 512 concurrency.
    @Sendable func multiple(request: Request, context: Context) async throws -> [World] {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        return try await withThrowingTaskGroup(of: World.self) { group in
            for _ in 0..<queries {
                group.addTask {
                    let id = Int32.random(in: 1...10_000)
                    let rows = try await self.postgresClient.query("SELECT id, randomnumber FROM World WHERE id = \(id)")
                    for try await (id, randomNumber) in rows.decode((Int32, Int32).self, context: .default) {
                        return World(id: id, randomNumber: randomNumber)
                    }
                    throw HTTPError(.notFound)
                }
            }
            var result: [World] = .init()
            result.reserveCapacity(queries)
            for try await world in group {
                result.append(world)
            }
            return result
        }
    }

    /// This test exercises database writes. Each request is processed by fetching 
    /// multiple rows from a simple database table, converting the rows to in-memory 
    /// objects, modifying one attribute of each object in memory, updating each 
    /// associated row in the database individually, and then serializing the list 
    /// of objects as a JSON response. The test is run multiple times: testing 1, 5, 
    /// 10, 15, and 20 updates per request. Note that the number of statements per 
    /// request is twice the number of updates since each update is paired with one 
    /// query to fetch the object. All tests are run at 512 concurrency.
    @Sendable func updates(request: Request, context: Context) async throws -> [World] {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        return try await withThrowingTaskGroup(of: World.self) { group in
            for _ in 0..<queries {
                group.addTask {
                    let id = Int32.random(in: 1...10_000)
                    let rows = try await self.postgresClient.query("SELECT id FROM World WHERE id = \(id)")
                    for try await (id) in rows.decode((Int32).self, context: .default) {
                        let randomNumber = Int32.random(in: 1...10_000)
                        try await self.postgresClient.query("UPDATE World SET randomnumber = \(randomNumber) WHERE id = \(id)")
                        return World(id: id, randomNumber: randomNumber)
                    }
                    throw HTTPError(.notFound)
                }
            }
            var result: [World] = .init()
            result.reserveCapacity(queries)
            for try await world in group {
                result.append(world)
            }
            return result
        }
    }
}
