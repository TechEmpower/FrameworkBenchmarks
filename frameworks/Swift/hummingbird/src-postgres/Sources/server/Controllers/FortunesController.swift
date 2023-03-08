import Hummingbird
import HummingbirdMustache
import PostgresNIO

struct HTML: HBResponseGenerator {
    let html: String
    public func response(from request: HBRequest) -> HBResponse {
        let buffer = request.allocator.buffer(string: html)
        return HBResponse(status: .ok, headers: ["content-type": "text/html; charset=utf-8"], body: .byteBuffer(buffer))
    }
}

class FortunesController {
    let connectionPoolGroup: HBConnectionPoolGroup<PostgresConnectionSource>
    let template: HBMustacheTemplate

    init(connectionPoolGroup: HBConnectionPoolGroup<PostgresConnectionSource>) {
        self.connectionPoolGroup = connectionPoolGroup
        self.template = try! HBMustacheTemplate(string: """
        <!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{{#.}}<tr><td>{{id}}</td><td>{{message}}</td></tr>{{/.}}</table></body></html>
        """)
    }

    func add(to router: HBRouterBuilder) {
        router.get("fortunes", use: fortunes)
    }

    func fortunes(request: HBRequest) -> EventLoopFuture<HTML> {
        return self.connection(for: request) { connection in 
            return connection.query("SELECT id, message FROM Fortune")
        }.flatMapThrowing { results in
            var fortunes = try results.map { result -> Fortune in
                let decoded = try result.decode((Int32, String).self, context: .default)
                return Fortune(id: decoded.0, message: decoded.1)
            }
            fortunes.append(.init(id: 0, message: "Additional fortune added at request time."))
            let sortedFortunes = fortunes.sorted { $0.message < $1.message }
            return HTML(html: self.template.render(sortedFortunes) )
        }
    }

    @discardableResult func connection<NewValue>(for request: HBRequest, closure: @escaping (PostgresConnection) -> EventLoopFuture<NewValue>) -> EventLoopFuture<NewValue> {
        return self.connectionPoolGroup.lease(on: request.eventLoop, logger: request.logger, process: closure)
    }
}
