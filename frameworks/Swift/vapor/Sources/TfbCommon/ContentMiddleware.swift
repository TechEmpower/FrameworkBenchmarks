import HTTP

/// Middleware that adds `Server` HTTP header to response.
public final class ContentMiddleware: Middleware {
    
    public init() { }
    
    public func respond(to request: Request, chainingTo next: Responder) throws -> Response {
        let response = try next.respond(to: request)

        if response.headers[HeaderKey.contentType] == "text/plain; charset=utf-8" {
            response.headers[HeaderKey.contentType] = "text/plain"
        } else if response.headers[HeaderKey.contentType] == "application/json; charset=utf-8" {
            response.headers[HeaderKey.contentType] = "application/json"
        }
        
        return response
    } 
}
