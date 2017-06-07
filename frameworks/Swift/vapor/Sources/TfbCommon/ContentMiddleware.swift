import HTTP

/// Middleware that adds `Server` HTTP header to response.
public final class ContentMiddleware: Middleware {
    
    public init() { }
    
    public func respond(to request: Request, chainingTo next: Responder) throws -> Response {
        let response = try next.respond(to: request)
        
        response.headers[HeaderKey.contentType] = response.headers[HeaderKey.contentType]?.replacingOccurrences(of: "; charset=utf-8", with: "")
        
        return response
    } 
}
