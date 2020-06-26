import PerfectHTTP
import PerfectHTTPServer
import PerfectLib
import Foundation

func plaintextHandler(request: HTTPRequest, response: HTTPResponse) {

	response.appendBody(string: "Hello, World!")

    setHeaders(response: response, contentType: "text/plain")

	response.completed()
}

func jsonHandler(request: HTTPRequest, response: HTTPResponse) {

    var helloDictionary: [String: String] = [:]
    helloDictionary["message"] = "Hello, World!"

    let errorPayload: [String: Any] = [
        "error": "Could not set body!"
    ]

    var responseString: String = ""
    var errorString: String = ""
    do {
        errorString = try errorPayload.jsonEncodedString()
    } catch {
        // Nothing to do here - we already have an empty value
    }

    do {
        responseString = try helloDictionary.jsonEncodedString()
        response.appendBody(string: responseString)
    } catch {
        response.status = HTTPResponseStatus.internalServerError
        response.appendBody(string: errorString)
    }


    setHeaders(response: response, contentType: "application/json")
    response.completed()
}

// Helpers

func setHeaders(response: HTTPResponse, contentType: String) {

    response.setHeader(.contentType, value: contentType)
    response.setHeader(.custom(name: "Server"), value: "Perfect")

    let currDate: String = getCurrDate()

	response.setHeader(.custom(name: "Date"), value: currDate)
}

func getCurrDate() -> String {

    let now = getNow()

    do {
        let formatted = try formatDate(now, format: "%a, %d %b %Y %H:%M:%S %Z", timezone: nil, locale: nil)
        return formatted
    } catch {
        return "error formatting date string"
    }
}

var routes = Routes()
routes.add(method: .get, uri: "/json", handler: jsonHandler)
routes.add(method: .get, uri: "/plaintext", handler: plaintextHandler)
routes.add(method: .get, uri: "/**",
		   handler: StaticFileHandler(documentRoot: "./webroot", allowResponseFilters: true).handleRequest)
try HTTPServer.launch(name: "localhost",
    port: 8080,
    routes: routes,
    responseFilters: [
    (PerfectHTTPServer.HTTPFilter.contentCompression(data: [:]), HTTPFilterPriority.high)])
