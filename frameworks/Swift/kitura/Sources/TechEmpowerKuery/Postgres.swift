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
import LoggerAPI
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

let dbRows = 10000
let maxValue = 10000

class World: Table {
    let tableName = "World"
    
    let id = Column("id")
    let randomNumber = Column("randomNumber")
}

class Fortunes: Table {
    let tableName = "Fortune"

    let id = Column("id")
    let message = Column("message")
}

let world = World()
let fortunes = Fortunes()

var update = Update(world, set: [(world.randomNumber, randomNumberGenerator(maxValue))])
    .where(world.id == randomNumberGenerator(dbRows))


let dbConnPoolOpts = ConnectionPoolOptions(initialCapacity: 20, maxCapacity: 50, timeout:10000)

func releaseConnection(connection: Connection) {
    connection.closeConnection()
}

func generateConnection() -> Connection? {
    var dbConn: Connection
    dbConn = PostgreSQLConnection(host: dbHost, port: dbPort,
                                  options: [.databaseName(dbName),
                                            .userName(dbUser), .password(dbPass) ])

    
    dbConn.connect() { error in
        if let error = error {
            print(error)
            return
        }
    }
    return dbConn
}

let dbConnPool = ConnectionPool(options: dbConnPoolOpts, connectionGenerator: generateConnection, connectionReleaser:releaseConnection)

// Return a random number within the range of rows in the database
func randomNumberGenerator(_ maxVal: Int) -> Int {
    #if os(Linux)
        return Int(random() % maxVal) + 1
    #else
        return Int(arc4random_uniform(UInt32(maxVal))) + 1
    #endif
}

func getFortunes() -> ([Fortune]?, AppError?) {
    var resultFortunes: [Fortune]? = nil
    var errRes: AppError? = nil
    
    // Get a dedicated connection object for this transaction from the pool
    guard let dbConn = dbConnPool.getConnection() else {
        errRes = AppError.OtherError("Timed out waiting for a DB connection from the pool")
        return (nil, errRes)
    }
    // Ensure that when we complete, the connection is returned to the pool
    defer {
        releaseConnection(connection: dbConn)
    }
    
    let query = Select(from: fortunes)

    dbConn.execute(query: query) { result in
        guard let rows = result.asRows, result.success else {
            errRes = AppError.DBKueryError("Query failed - status \(String(describing: result.asError))")
            return
        }
        do {
            resultFortunes = try rows.map { try Fortune.init(row: $0) }
        } catch {
            errRes = AppError.DataFormatError("\(error)")
        }
    }

    return (resultFortunes, errRes)
}

// Get a random row (range 1 to 10,000) from DB: id(int),randomNumber(int)
// Convert to object using object-relational mapping (ORM) tool
// Serialize object to JSON - example: {"id":3217,"randomNumber":2149}
func getRandomRow() -> ([String:Int]?, AppError?) {
    var resultDict: [String:Int]? = nil
    var errRes: AppError? = nil
    // Get a dedicated connection object for this transaction from the pool
    guard let dbConn = dbConnPool.getConnection() else {
        errRes = AppError.OtherError("Timed out waiting for a DB connection from the pool")
        return (resultDict, errRes)
    }
    // Ensure that when we complete, the connection is returned to the pool
    defer {
        releaseConnection(connection: dbConn)
    }
    let rnd = randomNumberGenerator(dbRows)
    
    let query = Select(world.randomNumber, from: world)
        .where(world.id == rnd)
    
    dbConn.execute(query: query) { result in
        if let resultSet = result.asResultSet {
            guard result.success else {
                errRes = AppError.DBKueryError("Query failed - status \(String(describing: result.asError))")
                return
            }
            
            for row in resultSet.rows {
                for value in row {
                    if let unwrapped = value {
                        guard let randomNumber = unwrapped as? Int32 else {
                        errRes = AppError.DBKueryError("Error: could not get field as an Int")
                        return
                    }
                        resultDict = ["id":rnd, "randomNumber":Int(randomNumber)]
                    } else {
                        errRes = AppError.DBKueryError("Error: randomNumber value is nil")
                    }

                }
                
            }
        }
    }
    return (resultDict, errRes)
}

// Updates a row of World to a new value.
func updateRow(id: Int) throws  -> AppError? {
    // Get a dedicated connection object for this transaction from the pool
    guard let dbConn = dbConnPool.getConnection() else {
        throw AppError.OtherError("Timed out waiting for a DB connection from the pool")
    }
    // Ensure that when we complete, the connection is returned to the pool
    defer {
        releaseConnection(connection: dbConn)
    }
    let rndValue = randomNumberGenerator(maxValue)
    let query = Update(world, set: [(world.randomNumber, rndValue)])
        .where(world.id == id)
    var errRes: AppError? = nil
    dbConn.execute(query: query) { result in
        if result.asResultSet != nil {
            guard result.success else {
                errRes = AppError.DBKueryError("Query failed - status \(String(describing: result.asError))")
                return
            }
            
        }
        
    }
    return errRes
}



