import Hummingbird
import HummingbirdMustache
import PostgresKit

struct HTML: HBResponseGenerator {
    let html: String
    public func response(from request: HBRequest) -> HBResponse {
        let buffer = request.allocator.buffer(string: html)
        return HBResponse(status: .ok, headers: ["content-type": "text/html; charset=utf-8"], body: .byteBuffer(buffer))
    }
}

class FortunesController {
    let template: HBMustacheTemplate

    init() {
        self.template = try! HBMustacheTemplate(string: """
        <!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{{#.}}<tr><td>{{id}}</td><td>{{message}}</td></tr>{{/.}}</table></body></html>
        """)
    }

    func add(to router: HBRouter) {
        router.get("fortunes", use: fortunes)
    }

    func fortunes(request: HBRequest) -> EventLoopFuture<HTML> {
        return request.db.query("SELECT id, message FROM Fortune").map { results in
            var fortunes = results.map {
                return Fortune(
                    id: $0.column("id")?.int32 ?? 0,
                    message: $0.column("message")?.string ?? ""
                )
            }
            fortunes.append(.init(id: 0, message: "Additional fortune added at request time."))
            let sortedFortunes = fortunes.sorted { $0.message < $1.message }
            return HTML(html: self.template.render(sortedFortunes) )
        }
    }
}
