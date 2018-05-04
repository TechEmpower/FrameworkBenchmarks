import Vapor

/// Middleware that adds `Server` HTTP header to response.
public final class ServerMiddleware: Middleware, ServiceType {

    public init() { }
    
    public func respond(to request: Request, chainingTo next: Responder) throws -> Future<Response> {
        return try next.respond(to: request).map { res in
            res.http.headers.replaceOrAdd(name: .server, value: "Vapor")
            return res
        }
    }

    public static func makeService(for worker: Container) throws -> ServerMiddleware {
        return .init()
    }
}
