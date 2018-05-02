import Vapor

/// Middleware that adds `Server` HTTP header to response.
public final class ContentMiddleware: Middleware {

    public init() { }
    
    public func respond(to request: Request, chainingTo next: Responder) throws -> EventLoopFuture<Response> {
        return try next.respond(to: request).map { res in
            let contentType = res.http.headers[.contentType].first!
            if contentType == "text/plain; charset=utf-8" {
                res.http.headers.replaceOrAdd(name: .contentType, value: "text/plain")
            } else if contentType == "application/json; charset=utf-8" {
                res.http.headers.replaceOrAdd(name: .contentType, value: "application/json")
            }
            return res
        }
    } 
}
