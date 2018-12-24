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
import TechEmpowerCommon

let router = Router()

//
// TechEmpower test 6: plaintext
//
router.get("/plaintext") {
    request, response, next in
    response.headers["Server"] = "Kitura"
    response.headers["Content-Type"] = "text/plain"
    try response.status(.OK).send("Hello, world!").end()
}

//
// TechEmpower test 1: JSON serialization
//
router.get("/json") {
    request, response, next in
    response.headers["Server"] = "Kitura"
    let result = ["message":"Hello, World!"]
    try response.status(.OK).send(json: result).end()
}

Kitura.addHTTPServer(onPort: 8080, with: router)
Kitura.run()
