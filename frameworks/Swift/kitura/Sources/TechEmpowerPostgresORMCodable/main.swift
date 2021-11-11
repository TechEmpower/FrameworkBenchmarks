/*
 * Copyright IBM Corporation 2018
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import Foundation
import Kitura
import LoggerAPI
import HeliumLogger
import KituraStencil
import Stencil
import TechEmpowerCommon
import KueryPostgres
import SwiftKueryORM
import KueryPostgresORM

Log.logger = HeliumLogger(.info)

// Stencil stuff
let ext = Extension()

// Stencil does not yet support automatic HTML escaping:
// https://github.com/kylef/Stencil/pull/80
//
ext.registerFilter("htmlencode") { (value: Any?) in
    if let value = value as? String {
        return value
            .replacingOccurrences(of: "&", with: "&amp;")
            .replacingOccurrences(of: "<", with: "&lt;")
            .replacingOccurrences(of: ">", with: "&gt;")
            .replacingOccurrences(of: "'", with: "&apos;")
            .replacingOccurrences(of: "\"", with: "&quot;")
    }
    return value
}

let router = Router()
router.add(templateEngine: StencilTemplateEngine(extension: ext))

// Configure our ORM Database connection pool as dbConnPool created by KueryPostgres
Database.default = Database(dbConnPool)

// Define the query parameters we can receive
struct TFBParams: QueryParams {
    let queries: Int

    // Override default decode to cater for the query parameter specification:
    //   If the parameter is missing, is not an integer, or is an integer less
    //   than 1, the value should be interpreted as 1.
    // This means that rather than failing to decode on a non-integer value, we
    // should fall back to a value of 1.
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        if let value = try? container.decode(Int.self, forKey: CodingKeys.queries) {
            self.queries = value
        } else {
            self.queries = 1
        }
    }
}

// Set Server header on all responses as per TechEmpower spec
router.all("/*") {
    _, response, next in
    response.headers["Server"] = "Kitura"
    next()
}

//
// TechEmpower test 2: Single database query (full ORM)
//
router.get("/db") { (respondWith: @escaping (RandomRow?, RequestError?) -> Void) in
    // Select random row from database range
    RandomRow.find(id: RandomRow.randomId, respondWith)
}

//
// TechEmpower test 3: Multiple database queries (full ORM)
// Get param provides number of queries: /queries?queries=N
//
router.get("/queries") { (params: TFBParams, respondWith: @escaping ([RandomRow]?, RequestError?) -> Void) in
    let numQueries = max(1, min(params.queries, 500))  // Snap to range of 1-500 as per test spec
    getRandomRows(count: numQueries, completion: respondWith)
}
router.get("/queriesParallel") { (params: TFBParams, respondWith: @escaping ([RandomRow]?, RequestError?) -> Void) in
    let numQueries = max(1, min(params.queries, 500))  // Snap to range of 1-500 as per test spec
    getRandomRowsParallel(count: numQueries, completion: respondWith)
}

//
// TechEmpower test 4: fortunes (full ORM)
// TODO: convert to Codable once templating support is available
//
router.get("/fortunes") {
    request, response, next in
    response.headers["Content-Type"] = "text/html; charset=UTF-8"
    Fortune.findAll { (fortunes, err) in
        if var fortunes = fortunes {
            fortunes.append(Fortune(id: 0, message: "Additional fortune added at request time."))
            do {
                try response.render("fortunes.stencil", context: ["fortunes": fortunes.sorted()]).end()
            } catch {
                try? response.status(.internalServerError).send("Error: \(error)").end()
            }
        } else {
            try? response.status(.internalServerError).send("Error: \(err ?? .internalServerError)").end()
        }
    }
}

//
// TechEmpower test 5: updates (full ORM)
//
router.get("/updates") { (params: TFBParams, respondWith: @escaping ([RandomRow]?, RequestError?) -> Void) in
    let numQueries = max(1, min(params.queries, 500))  // Snap to range of 1-500 as per test spec
    updateRandomRows(count: numQueries, completion: respondWith)
}
router.get("/updatesParallel") { (params: TFBParams, respondWith: @escaping ([RandomRow]?, RequestError?) -> Void) in
    let numQueries = max(1, min(params.queries, 500))  // Snap to range of 1-500 as per test spec
    updateRandomRowsParallel(count: numQueries, completion: respondWith)
}


Kitura.addHTTPServer(onPort: 8080, with: router)
Kitura.run()
