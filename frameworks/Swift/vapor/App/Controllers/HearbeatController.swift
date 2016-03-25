import Vapor

class HeartbeatController: ResourceController, DefaultInitializable {
    required init() {

    }

	/// List all resources
	func index(request: Request) throws -> ResponseConvertible {
		return try Json(["lub": "dub"])
	}

	/// Create a new instance.
    func store(request: Request) throws -> ResponseConvertible {
        return ""
    }

    /// Show an instance.
    func show(request: Request) throws -> ResponseConvertible {
        return ""
    }

    /// Update an instance.
    func update(request: Request) throws -> ResponseConvertible {
        return ""
    }

    /// Delete an instance.
    func destroy(request: Request) throws -> ResponseConvertible {
        return ""
    }

}
