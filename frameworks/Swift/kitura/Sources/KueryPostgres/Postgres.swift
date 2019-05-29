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
import SwiftKuery
import SwiftKueryPostgreSQL
import Configuration

#if os(Linux)
    import Glibc
#else
    import Darwin
#endif

// We will load our database configuration from config.json, but this can be
// overridden with the TFB_DB_CONFIG environment variable.
let configurationFilename: String = ProcessInfo.processInfo.environment["TFB_DB_CONFIG"] ?? "config.json"
let manager = ConfigurationManager().load(file: configurationFilename, relativeFrom: .pwd).load(.environmentVariables)

let dbHost = manager["DB_HOST"] as? String ?? manager["db:host"] as? String ?? "localhost"
let dbPort = Int32(manager["DB_PORT"] as? String != nil ? Int(manager["DB_PORT"] as! String) ?? 5432 : manager["db:port"] as? Int ?? 5432)
let dbName = manager["db:name"] as? String ?? "hello_world"
let dbUser = manager["db:user"] as? String ?? "benchmarkdbuser"
let dbPass = manager["db:password"] as? String ?? "benchmarkdbpass"

let dbConnPoolOpts = ConnectionPoolOptions(initialCapacity: 20, maxCapacity: 50)

public let dbConnPool = PostgreSQLConnection.createPool(host: dbHost, port: dbPort, options: [.databaseName(dbName), .userName(dbUser), .password(dbPass)], poolOptions: dbConnPoolOpts)

