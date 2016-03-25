import Vapor

class SampleMiddleware: Middleware {

	class func handle(handler: Request.Handler, for application: Application) -> Request.Handler {
		return { request in
			// You can manipulate the request before calling the handler
			// and abort early if necessary, a good injection point for
			// handling auth.

			// return Response(status: .Forbidden, text: "Permission denied")

			let response = try handler(request: request)

			// You can also manipulate the response to add headers
			// cookies, etc.

			return response
		}
	}

}
