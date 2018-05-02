import Vapor

/// Middleware that adds `Server` HTTP header to response.
public final class ServerMiddleware: Middleware {
    
    public init() { }
    
    public func respond(to request: Request, chainingTo next: Responder) throws -> Future<Response> {
        return try next.respond(to: request).map { res in
            res.http.headers.replaceOrAdd(name: .server, value: "Vapor")
            return res
        }
    } 
}
