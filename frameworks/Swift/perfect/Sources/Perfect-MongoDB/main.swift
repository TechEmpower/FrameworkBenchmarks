import PerfectHTTP
import PerfectHTTPServer
import PerfectLib
import PerfectMongoDB
import Foundation

let tfbHost = "tfb-database"
let database = "hello_world"
let username = "benchmarkdbuser"
let password = "benchmarkdbpass"

let client = try! MongoClient(uri: "mongodb://\(tfbHost)")
let db = client.getDatabase(name: database)
let World = db.getCollection(name: "world")
let Fortune = db.getCollection(name: "fortune")

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

func fetchFromWorld(id: String?) -> [String: Any] {

    var rand:Int = 0

    if id == nil {
        rand = numGenerator.random() % 10000 + 1
    } else {
        rand = Int(id!)!
    }

    if let world = World {

        var json = [String:Any]()
        json["id"] = rand

        var fields = [String: Any]()
        fields["id"] = 1
        fields["randomNumber"] = 1
        fields["_id"] = 0

        var fieldString: String = ""

        do {
            fieldString = try fields.jsonEncodedString()
        } catch {
            fieldString = String(describing: fields)
        }

        do {
            let jsonString = try json.jsonEncodedString()
            do {
                let results = try world.find(query: BSON( json: jsonString ), fields: BSON( json: fieldString ))

                if let res = results {
                    for item in res {
                        let itemString = String(describing: item)
                        return convertStringToDictionary(str: itemString)
                    }
                } else {
                    print("results couldn't be unwrapped: ", rand)
                }
            } catch {
                //
            }
        } catch {
            // empty on purpose
        } 
    } else {
        //
    }
    
    let emptyObj = [String: Any]()
    return emptyObj
}

func updateOneFromWorld() -> [String: Any] {

    let worldToUpdate = fetchFromWorld(id: nil)
    var id = Int()
    if worldToUpdate["id"] != nil {
        id = worldToUpdate["id"] as! Int
    } else {
        id = 1
        print("Error trying to fetch a world to update")
    }
    let newRandom = numGenerator.random() % 10000
    var errorObj = [String: Any]()

    if let world = World {

        var json = [String: Any]()
        json["id"] = id

        var fields = [String: Any]()
        fields["id"] = 1
        fields["randomNumber"] = 1
        fields["_id"] = 0

        var update = [String: Any]()
        update["randomNumber"] = newRandom

        var fieldString: String = ""

        do {
            fieldString = try fields.jsonEncodedString()
        } catch {
            fieldString = String(describing: fields)
        }

        var updateString: String = ""
        var jsonString: String = ""

        do {
            updateString = try update.jsonEncodedString()
        } catch {
            updateString = String(describing: update)
        }

        do {
            jsonString = try json.jsonEncodedString()
        } catch {
            jsonString = String(describing: json)
        }

        do {
            let results = try world.findAndModify(query: BSON( json: jsonString ), sort: nil, update: BSON( json: updateString ), fields: BSON( json: fieldString ), remove: false, upsert: false, new: true)
            let resultsStr = String(describing: results)
            return convertUpdateStringToDictionary(str: resultsStr, id: id)
        } catch {
            errorObj["id"] = "Error running query findAndModify"
            return errorObj
        }
    } else {
        errorObj["id"] = "world is empty"
        return errorObj
    }
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

    var queryArr = [[String: Any]]()

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

    do {
        response.appendBody(string: try res.jsonEncodedString())
    } catch {
        response.appendBody(string: String(describing: res))
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

func convertStringToDictionary(str: String) -> [String: Any] {

    let strOfWordsArray = str.components(separatedBy: " ")

    var returnObj = [String: Any]()

    returnObj["id"] = Int(strOfWordsArray[3].dropLast())
    returnObj["randomNumber"] = Int(strOfWordsArray[6])

    return returnObj
}

func convertUpdateStringToDictionary(str: String, id: Int) -> [String: Any] {

    let strOfWordsArray = str.components(separatedBy: " ")

    var returnObj = [String: Any]()
    returnObj["id"] = id
    returnObj["randomNumber"] = Int(strOfWordsArray[16])

    return returnObj
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
