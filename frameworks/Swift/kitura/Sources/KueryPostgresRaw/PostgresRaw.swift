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
import KueryPostgres
import TechEmpowerCommon

let dbRows = 10000
let maxValue = 10000

// Kuery table definition for World
class World: Table {
    let tableName = "world"
    
    let id = Column("id")
    let randomNumber = Column("randomnumber")
}

// Kuery table definition for Fortune
class Fortunes: Table {
    let tableName = "fortune"

    let id = Column("id")
    let message = Column("message")
}

let world = World()
let fortunes = Fortunes()

// Kuery update statement for Updates
var update = Update(world, set: [(world.randomNumber, RandomRow.randomValue)])
    .where(world.id == RandomRow.randomId)

/// Get a list of Fortunes from the database.
///
/// - Parameter callback: The callback that will be invoked once the DB query
///                       has completed and results are available, passing an
///                       optional [Fortune] (on success) or AppError on
///                       failure.
///
public func getFortunes(callback: @escaping ([Fortune]?, AppError?) -> Void) -> Void {
    // Get a dedicated connection object for this transaction from the pool
    guard let dbConn = dbConnPool.getConnection() else {
        return callback(nil, AppError.OtherError("Timed out waiting for a DB connection from the pool"))
    }
    // Initiate database query
    let query = Select(from: fortunes)
    dbConn.execute(query: query) { result in
        var resultFortunes: [Fortune]? = nil
        guard let rows = result.asRows, result.success else {
            return callback(nil, AppError.DBKueryError("Query failed: \(String(describing: result.asError))"))
        }
        do {
            resultFortunes = try rows.map { try Fortune.init(row: $0) }
        } catch {
            return callback(nil, AppError.DataFormatError("\(error)"))
        }
        // Invoke callback with results
        callback(resultFortunes, nil)
    }

}

/// Get a random row (range 1 to 10,000) from the database.
///
/// - Parameter callback: The callback that will be invoked once the DB query
///                       has completed and results are available, passing an
///                       optional RandomRow (on success) or AppError on
///                       failure.
///
public func getRandomRow_Raw(callback: @escaping (RandomRow?, AppError?) -> Void) -> Void {
    // Get a dedicated connection object for this transaction from the pool
    guard let dbConn = dbConnPool.getConnection() else {
        return callback(nil, AppError.OtherError("Timed out waiting for a DB connection from the pool"))
    }
    // Select random row from database range
    let rnd = RandomRow.randomId
    let query = Select(world.randomNumber, from: world)
        .where(world.id == rnd)
    // Initiate database query
    dbConn.execute(query: query) { result in
        var resultRow: RandomRow? = nil
        guard let resultSet = result.asResultSet, result.success else {
            return callback(nil, AppError.DBKueryError("Query failed: \(String(describing: result.asError))"))
        }
            
        for row in resultSet.rows {
            for value in row {
                guard let value = value else {
                    return callback(nil, AppError.DBKueryError("Error: randomNumber value is nil"))
                }
                guard let randomNumber = value as? Int32 else {
                    return callback(nil, AppError.DBKueryError("Error: could not convert \(value) to Int32"))
                }
                resultRow = RandomRow(id: rnd, randomNumber: Int(randomNumber))
            }
        }
        // Invoke callback with results
        callback(resultRow, nil)
    }
}

/// Updates a row of World to a new value.
///
/// - Parameter callback: The callback that will be invoked once the DB update
///                       has completed, passing an optional AppError if the
///                       update failed.
///
public func updateRow_Raw(id: Int, callback: @escaping (AppError?) -> Void) -> Void {
    // Get a dedicated connection object for this transaction from the pool
    guard let dbConn = dbConnPool.getConnection() else {
        return callback(AppError.OtherError("Timed out waiting for a DB connection from the pool"))
    }
    // Generate a random number for this row
    let rndValue = RandomRow.randomValue
    let query = Update(world, set: [(world.randomNumber, rndValue)])
        .where(world.id == id)
    // Initiate database query
    dbConn.execute(query: query) { result in
        guard result.success else {
            return callback(AppError.DBKueryError("Update failed: \(String(describing: result.asError))"))
        }
        // Invoke callback once done
        callback(nil)
    }
}

/// Get `count` random rows from the database, and pass the resulting array
/// to a completion handler (or an AppError, in the event that a row could
/// not be retrieved).
///
/// - Parameter count: The number of rows to retrieve
/// - Parameter result: The intermediate result array being built
/// - Parameter completion: The closure to invoke with the result array, or error
///
public func getRandomRows_Raw(count: Int, result: [RandomRow] = [], completion: @escaping ([RandomRow]?, AppError?) -> Void) {
    if count > 0 {
        // Select random row from database range
        getRandomRow_Raw { (resultRow, err) in
            if let resultRow = resultRow {
                var result = result
                result.append(resultRow)
                // Call recursively to get remaining rows
                getRandomRows_Raw(count: count-1, result: result, completion: completion)
            } else {
                if let err = err {
                    completion(nil, err)
                } else {
                    fatalError("Unexpected: result and error both nil")
                }
            }
        }
    } else {
        completion(result, nil)
    }
}

/// Update and retrieve `count` random rows from the database, and pass the
/// resulting array to a completion handler (or an AppError, in the event
/// that a row could not be retrieved or updated).
///
/// - Parameter count: The number of rows to retrieve
/// - Parameter result: The intermediate result array being built
/// - Parameter completion: The closure to invoke with the result array, or error
///
public func updateRandomRows_Raw(count: Int, result: [RandomRow] = [], completion: @escaping ([RandomRow]?, AppError?) -> Void) {
    if count > 0 {
        // Select random row from database range
        getRandomRow_Raw { (resultRow, err) in
            if let resultRow = resultRow {
                var result = result
                // Execute inner callback for updating the row
                updateRow_Raw(id: resultRow.id) { (err) in
                    if let err = err {
                        return completion(nil, err)
                    }
                    result.append(resultRow)
                    // Call recursively to update remaining rows
                    updateRandomRows_Raw(count: count-1, result: result, completion: completion)
                }
            } else {
                if let err = err {
                    completion(nil, err)
                } else {
                    fatalError("Unexpected: result and error both nil")
                }
            }
        }
    } else {
        completion(result, nil)
    }
}
