import Vapor

struct ServerMiddleware: Middleware {
    func respond(to request: Request, chainingTo next: Responder) -> EventLoopFuture<Response> {
        next.respond(to: request).map { response in
            response.headers.add(name: .server, value: "Vapor")
            return response
        }
    }
}
