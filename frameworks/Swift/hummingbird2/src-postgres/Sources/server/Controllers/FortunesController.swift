import Hummingbird
import Mustache
import PostgresNIO

struct HTML: ResponseGenerator, Sendable {
    let html: String
    public func response(from request: Request, context: some RequestContext) -> Response {
        let buffer = context.allocator.buffer(string: html)
        return Response(status: .ok, headers: [.contentType: "text/html; charset=utf-8"], body: .init(byteBuffer: buffer))
    }
}

final class FortunesController: Sendable {
    typealias Context = TechFrameworkRequestContext
    let template: MustacheTemplate
    let postgresClient: PostgresClient

    init(postgresClient: PostgresClient) {
        self.postgresClient = postgresClient
        self.template = try! MustacheTemplate(string: """
        <!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{{#.}}<tr><td>{{id}}</td><td>{{message}}</td></tr>{{/.}}</table></body></html>
        """)
    }

    var routes: RouteCollection<Context> {
        RouteCollection(context: Context.self)
            .get("fortunes", use: fortunes)
    }

    /// In this test, the framework's ORM is used to fetch all rows from a database
    ///  table containing an unknown number of Unix fortune cookie messages (the 
    /// table has 12 rows, but the code cannot have foreknowledge of the table's 
    /// size). An additional fortune cookie message is inserted into the list at 
    /// runtime and then the list is sorted by the message text. Finally, the list 
    /// is delivered to the client using a server-side HTML template. The message 
    /// text must be considered untrusted and properly escaped and the UTF-8 fortune messages must be rendered properly.
    @Sendable func fortunes(request: Request, context: Context) async throws -> HTML {
        let rows = try await self.postgresClient.execute(SelectFortuneStatement())
        var fortunes: [Fortune] = []
        for try await fortune in rows {
            fortunes.append(.init(id: fortune.0, message: fortune.1))
        }

        fortunes.append(.init(id: 0, message: "Additional fortune added at request time."))
        let sortedFortunes = fortunes.sorted { $0.message < $1.message }
        return HTML(html: self.template.render(sortedFortunes) )
        
    }

    struct SelectFortuneStatement: PostgresPreparedStatement {
        typealias Row = (Int32, String)

        static var sql = "SELECT id, message FROM Fortune"

        func makeBindings() throws -> PostgresNIO.PostgresBindings {
            return .init()
        }

        func decodeRow(_ row: PostgresNIO.PostgresRow) throws -> Row { try row.decode(Row.self) }
    }
}
