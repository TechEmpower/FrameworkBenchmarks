import Hummingbird
import Mustache
import PostgresKit

struct HTML: HBResponseGenerator {
    let html: String
    public func response(from request: HBRequest) -> HBResponse {
        let buffer = request.allocator.buffer(string: html)
        return HBResponse(status: .ok, headers: ["content-type": "text/html; charset=utf-8"], body: .byteBuffer(buffer))
    }
}

class FortunesController {
    let template: Template

    init() {
        self.template = try! Template(string: """
        !<!DOCTYPE html>
        <html>
        <head><title>Fortunes</title></head>
        <body>
        <table>
        <tr><th>id</th><th>message</th></tr>
        {{#.}}
        <tr><td>{{id}}</td><td>{{.}}</td></tr>
        {{/.}}
        </table>
        </body>
        </html>
        """)
    }

    func add(to router: HBRouter) {
        router.get("fortunes", use: fortunes)
    }

    func fortunes(request: HBRequest) -> EventLoopFuture<HTML> {
        return request.db.query("SELECT id, message FROM Fortunes").map { results in
            var fortunes = results.map {
                return $0.column("message")?.string ?? ""
            }
            fortunes.append("Additional fortune added at request time.")
            let sortedFortunes = fortunes.sorted { $0 < $1 }
            return try! HTML(html: self.template.render(sortedFortunes) )
        }
/*        var fortunes = ["Hello", "Goodbye"]
        fortunes.append("Additional fortune added at request time.")
        let sortedFortunes = fortunes.sorted { $0 < $1 }
        return try! HTML(html: self.template.render(sortedFortunes) )     //request.response.headers.replaceOrAdd(name: "content-type", value: "text/html; charset=UTF-8")
        //return try! template.render([["message": "<tesét>"], ["message": "goodbye"]])
*/    }
}
