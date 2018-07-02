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

import Kitura
import LoggerAPI
import HeliumLogger
import KituraMustache
import TechEmpowerCommon
import KueryPostgresRaw

Log.logger = HeliumLogger(.info)

let router = Router()
router.add(templateEngine: MustacheTemplateEngine())

//
// TechEmpower test 4: fortunes (raw, no ORM)
//
router.get("/fortunes") {
    request, response, next in
    response.headers["Server"] = "Kitura"
    response.headers["Content-Type"] = "text/html; charset=UTF-8"
    getFortunes { (fortunes, err) in
        guard var fortunes = fortunes else {
            guard let err = err else {
                Log.error("Unknown Error")
                try? response.status(.badRequest).send("Unknown error").end()
                return
            }
            Log.error("\(err)")
            try? response.status(.badRequest).send("Error: \(err)").end()
            return
        }
        fortunes.append(Fortune(id: 0, message: "Additional fortune added at request time."))
        do {
          try response.render("fortunes.mustache", context: ["fortunes": fortunes.sorted()]).end()
        } catch {
          print("Error: \(error)")
        }
    }
}

Kitura.addHTTPServer(onPort: 8080, with: router)
Kitura.run()
