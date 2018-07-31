import Vapor

struct Message: Content {
    let message: String?

    init(message: String? = nil) {
        self.message = message
    }
}
