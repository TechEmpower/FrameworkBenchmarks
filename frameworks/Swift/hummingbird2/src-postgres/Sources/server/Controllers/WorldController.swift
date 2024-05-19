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
        let rows = try await self.postgresClient.execute(SelectWorldStatement(id: id))
        guard let row = try await rows.first(where: {_ in true }) else {
            throw HTTPError(.notFound)
        }
        return World(id: row.0, randomNumber: row.1)
    }

    /// In this test, each request is processed by fetching multiple rows from a 
    /// simple database table and serializing these rows as a JSON response. The 
    /// test is run multiple times: testing 1, 5, 10, 15, and 20 queries per request. 
    /// All tests are run at 512 concurrency.
    @Sendable func multiple(request: Request, context: Context) async throws -> [World] {
        let queries = (request.uri.queryParameters.get("queries", as: Int.self) ?? 1).bound(1, 500)
        return try await self.postgresClient.withConnection { conn in
            var result: [World] = .init()
            result.reserveCapacity(queries)
            for _ in 0..<queries {
                let id = Int32.random(in: 1...10_000)
                let rows = try await conn.execute(SelectWorldStatement(id: id), logger: context.logger)
                guard let row = try await rows.first(where: {_ in true }) else {
                    throw HTTPError(.notFound)
                }
                result.append( World(id: row.0, randomNumber: row.1))
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
        return try await self.postgresClient.withConnection { conn in
            var result: [World] = .init()
            result.reserveCapacity(queries)
            for _ in 0..<queries {
                let id = Int32.random(in: 1...10_000)
                let rows = try await conn.execute(SelectWorldStatement(id: id), logger: context.logger)
                guard let row = try await rows.first(where: {_ in true }) else {
                    throw HTTPError(.notFound)
                }
                let randomNumber = Int32.random(in: 1...10_000)
                _ = try await conn.execute(UpdateWorldStatement(id: id, randomNumber: randomNumber), logger: context.logger)
                result.append(World(id: row.0, randomNumber: randomNumber))
            }
            return result
        }
    }

    struct SelectWorldStatement: PostgresPreparedStatement {
        typealias Row = (Int32, Int32)

        let id: Int32

        static var sql = "SELECT id, randomnumber FROM World WHERE id = $1"

        func makeBindings() throws -> PostgresNIO.PostgresBindings {
            var bindings = PostgresNIO.PostgresBindings(capacity: 1)
            bindings.append(.init(int32: self.id))
            return bindings
        }

        func decodeRow(_ row: PostgresNIO.PostgresRow) throws -> Row { try row.decode(Row.self) }
    }

    struct UpdateWorldStatement: PostgresPreparedStatement {
        typealias Row = Int32

        let id: Int32
        let randomNumber: Int32

        static var sql = "UPDATE World SET randomnumber = $2 WHERE id = $1"

        func makeBindings() throws -> PostgresNIO.PostgresBindings {
            var bindings = PostgresNIO.PostgresBindings(capacity: 2)
            bindings.append(.init(int32: self.id))
            bindings.append(.init(int32: self.randomNumber))
            return bindings
        }

        func decodeRow(_ row: PostgresNIO.PostgresRow) throws -> Row { try row.decode(Row.self) }
    }
}
