import HTTP

/// Middleware that adds `Server` HTTP header to response.
public final class ServerMiddleware: Middleware {
    
    public init() { }
    
    public func respond(to request: Request, chainingTo next: Responder) throws -> Response {
        let response = try next.respond(to: request)
        
        response.headers["Server"] = "Vapor"
        
        return response
    } 
}
