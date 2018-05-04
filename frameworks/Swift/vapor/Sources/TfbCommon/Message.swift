import Vapor

public struct Message: Content {
    public let message: String

    public init(_ message: String) {
        self.message = message
    }
}
