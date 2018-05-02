import Vapor
import Foundation
import TfbCommon
import FluentMySQL

struct EmptyJSON: Content {}

public func routes(_ router: Router) throws {

    router.get("json") { req in
        return Message("Hello, World!")
    }

    router.get("plaintext") { req in
        return "Hello, world!" as StaticString
    }

    // response to requests to /info domain
    // with a description of the request
    router.get("info") { req in
        return req.description
    }

    router.get("description") { req in
        return req.description
    }
    
}
