import PerfectHTTP
import PerfectHTTPServer
import PerfectLib
import PerfectPostgreSQL
import Foundation

let tfbHost = "tfb-database"
let database = "hello_world"
let username = "benchmarkdbuser"
let password = "benchmarkdbpass"

let p = PGConnection()
let status = p.connectdb("postgresql://\(username):\(password)@\(tfbHost):5432/\(database)")

class LinearCongruntialGenerator {
 
    var state = 0 //seed of 0 by default
    let a, c, m, shift: Int
 
    init() {
        self.a = 214013
        self.c = 2531011
        self.m = Int(pow(2.0, 31.0)) //2^31 or 2147483648
        self.shift = 16
    }
 
    func random() -> Int {
        state = (a * state + c) % m
        return state >> shift
    }
}

let numGenerator = LinearCongruntialGenerator()

func fetchFromWorld(id: String?) -> [String:Any] {

    var returnObj = [String: Any]()
    var rand:Int = 0

    if id == nil {
        rand = numGenerator.random() % 10000
    } else {
        rand = Int(id!)!
    }

    let results = p.exec(statement: "select id, randomNumber from world where id = \(rand)")

    returnObj["id"] = results.getFieldString(tupleIndex: 0, fieldIndex: 0)!
    returnObj["randomNumber"] = results.getFieldString(tupleIndex: 0, fieldIndex: 1)!
    
    return returnObj
}

func updateOneFromWorld() -> [String: Any] {

    var returnObj = [String: Any]()
    let worldToUpdate = fetchFromWorld(id: nil)
    let rand = numGenerator.random() % 10000 + 1
    let id: String = worldToUpdate["id"] as! String

    let _ = p.exec(statement: "UPDATE world SET randomNumber = \(rand) WHERE id = \(id)")

    returnObj["id"] = id
    returnObj["randomNumber"] = rand

    return returnObj
}

func updatesHandler(request: HTTPRequest, response: HTTPResponse) {

    let queryStr = returnCorrectTuple(queryArr: request.queryParams)
    var totalQueries = sanitizeQueryValue(queryString: queryStr)

    var updateArr: Array = [[String: Any]]()

    while 0 < totalQueries {
        updateArr.append(updateOneFromWorld())
        totalQueries -= 1
    }

    do {

        response.appendBody(string: try updateArr.jsonEncodedString())
    } catch {

        response.appendBody(string: String(describing: updateArr))
    }

    setHeaders(response: response, contentType: "application/json")

    response.completed()
}

func multipleDatabaseQueriesHandler(request: HTTPRequest, response: HTTPResponse) {

    let queryStr = returnCorrectTuple(queryArr: request.queryParams)
    var totalQueries = sanitizeQueryValue(queryString: queryStr)

    var queryArr: Array = [[String: Any]]()

    while 0 < totalQueries {

        queryArr.append(fetchFromWorld(id: nil))
        totalQueries -= 1
    }  

    do {

        response.appendBody(string: try queryArr.jsonEncodedString())
    } catch {

        response.appendBody(string: String(describing: queryArr))
    }

    setHeaders(response: response, contentType: "application/json")

    response.completed()
}

func singleDatabaseQueryHandler(request: HTTPRequest, response: HTTPResponse) {

    let res = fetchFromWorld(id: nil)

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
        responseString = try res.jsonEncodedString()
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

func returnCorrectTuple(queryArr: [(String, String)]) -> String {

    for tup in queryArr {
        if String(describing: tup.0) == "queries" {
            return String(describing: tup.1)
        }
    }

    return "nil"
}

func sanitizeQueryValue(queryString: String) -> Int {

    if let queryNum = Int(queryString) {

        if queryNum > 0 && queryNum < 500 {
            return queryNum
        } else if queryNum > 500 {
            return 500
        } else {
            return 1
        }
    } else {
        return 1
    }
}

func spoofHTML(fortunesArr: [[String: Any]]) -> String {

    var htmlToRet = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"

    for fortune in fortunesArr {

        htmlToRet += "<tr><td>\(fortune["id"]!)</td><td>\(fortune["message"]!)</td></tr>"
    }

    htmlToRet += "</table></body></html>";

    return htmlToRet
}

var routes = Routes()
routes.add(method: .get, uri: "/updates", handler: updatesHandler)
routes.add(method: .get, uri: "/queries", handler: multipleDatabaseQueriesHandler)
routes.add(method: .get, uri: "/db", handler: singleDatabaseQueryHandler)
routes.add(method: .get, uri: "/**",
		   handler: StaticFileHandler(documentRoot: "./webroot", allowResponseFilters: true).handleRequest)
try HTTPServer.launch(name: "localhost",
    port: 8080,
    routes: routes,
    responseFilters: [
    (PerfectHTTPServer.HTTPFilter.contentCompression(data: [:]), HTTPFilterPriority.high)])
