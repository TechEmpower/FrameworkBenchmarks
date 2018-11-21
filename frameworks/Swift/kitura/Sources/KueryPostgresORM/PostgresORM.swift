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
import Dispatch
import LoggerAPI
import SwiftKuery
import SwiftKueryORM
import KueryPostgres
import TechEmpowerCommon

// ORM conformance
extension RandomRow: Model {
    public static var tableName: String { return "world" }
}

// ORM conformance
extension Fortune: Model {
    public static var tableName: String { return "fortune" }
}

// Configure our ORM Database connection pool as dbConnPool created by KueryPostgres
public func setupORM() {
    Database.default = Database(dbConnPool)
}

/// Get a list of Fortunes from the database.
///
/// - Parameter callback: The callback that will be invoked once the DB query
///                       has completed and results are available, passing an
///                       optional [Fortune] (on success) or RequestError on
///                       failure.
///
public func getFortunes(callback: @escaping ([Fortune]?, RequestError?) -> Void) -> Void {
    Fortune.findAll { (fortunes, err) in
        if let err = err {
            return callback(nil, err)
        } else {
            callback(fortunes, nil)
        }
    }
}

/// Get a random row (range 1 to 10,000) from the database.
///
/// - Parameter callback: The callback that will be invoked once the DB query
///                       has completed and results are available, passing an
///                       optional RandomRow (on success) or RequestError on
///                       failure.
///
public func getRandomRow(callback: @escaping (RandomRow?, RequestError?) -> Void) -> Void {
    // Select random row from database range
    let rnd = RandomRow.randomId
    RandomRow.find(id: rnd, callback)
}

/// Updates a row of World to a new value.
///
/// - Parameter callback: The callback that will be invoked once the DB update
///                       has completed, passing an optional RequestError if the
///                       update failed.
///
public func updateRow(id: Int, callback: @escaping (RequestError?) -> Void) -> Void {
    // Generate a random number for this row
    let row = RandomRow(id: id, randomNumber: RandomRow.randomValue)
    row.update(id: id) { (resultRow, err) in
        if let err = err {
            return callback(err)
        } else {
            callback(nil)
        }
    }
}

/// Get `count` random rows from the database, and pass the resulting array
/// to a completion handler (or a RequestError, in the event that a row could
/// not be retrieved).
///
/// - Parameter count: The number of rows to retrieve
/// - Parameter result: The intermediate result array being built
/// - Parameter completion: The closure to invoke with the result array, or error
///
public func getRandomRows(count: Int, result: [RandomRow] = [], completion: @escaping ([RandomRow]?, RequestError?) -> Void) {
    if count > 0 {
        // Select random row from database range
        RandomRow.find(id: RandomRow.randomId) { (resultRow, err) in
            if let resultRow = resultRow {
                var result = result
                result.append(resultRow)
                getRandomRows(count: count-1, result: result, completion: completion)
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

/// A parallel version of `getRandomRows` that invokes each get in parallel, builds an
/// array of results and waits for each get to complete before returning.
///
/// - Parameter count: The number of rows to retrieve
/// - Parameter completion: The closure to invoke with the result array, or error
///
public func getRandomRowsParallel(count: Int, completion: @escaping ([RandomRow]?, RequestError?) -> Void) {
    var results: [RandomRow] = []
    guard count > 0 else {
        return completion(results, nil)
    }
    // Used to protect result array from concurrent modification
    let updateLock = DispatchSemaphore(value: 1)
    // Execute each query. Each callback will append its result to `results`
    for _ in 1...count {
        RandomRow.find(id: RandomRow.randomId) { (resultRow, err) in
            guard let resultRow = resultRow else {
                Log.error("\(err ?? .internalServerError)")
                completion(nil, err ?? .internalServerError)
                return
            }
            updateLock.wait()
            results.append(resultRow)
            if results.count == count {
                completion(results, nil)
            }
            updateLock.signal()
        }
    }
}

/// Update and retrieve `count` random rows from the database, and pass the
/// resulting array to a completion handler (or a RequestError, in the event
/// that a row could not be retrieved or updated).
///
/// - Parameter count: The number of rows to retrieve
/// - Parameter result: The intermediate result array being built
/// - Parameter completion: The closure to invoke with the result array, or error
///
public func updateRandomRows(count: Int, result: [RandomRow] = [], completion: @escaping ([RandomRow]?, RequestError?) -> Void) {
    if count > 0 {
        // Select random row from database range
        RandomRow.find(id: RandomRow.randomId) { (resultRow, err) in
            if let resultRow = resultRow {
                var result = result
                let row = RandomRow(id: resultRow.id, randomNumber: RandomRow.randomValue)
                row.update(id: row.id) { (resultRow, err) in
                    if let resultRow = resultRow {
                        result.append(resultRow)
                        updateRandomRows(count: count-1, result: result, completion: completion)
                    } else {
                        completion(nil, err)
                    }
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

/// A parallel version of `updateRandomRows` that invokes each get/update operation
/// in parallel, builds an array of results and waits for each get to complete before
/// returning.
///
/// - Parameter count: The number of rows to retrieve
/// - Parameter completion: The closure to invoke with the result array, or error
///
public func updateRandomRowsParallel(count: Int, completion: @escaping ([RandomRow]?, RequestError?) -> Void) {
    var results: [RandomRow] = []
    guard count > 0 else {
        return completion(results, nil)
    }
    // Used to protect result array from concurrent modification
    let updateLock = DispatchSemaphore(value: 1)
    // Execute each query. Each callback will append its result to `results`
    for _ in 1...count {
        RandomRow.find(id: RandomRow.randomId) { (resultRow, err) in
            guard let resultRow = resultRow else {
                Log.error("\(err ?? .internalServerError)")
                completion(nil, err ?? .internalServerError)
                return
            }
            let row = RandomRow(id: resultRow.id, randomNumber: RandomRow.randomValue)
            row.update(id: row.id) { (resultRow, err) in
                if let resultRow = resultRow {
                    updateLock.wait()
                    results.append(resultRow)
                    if results.count == count {
                        completion(results, nil)
                    }
                    updateLock.signal()
                } else {
                    Log.error("\(err ?? .internalServerError)")
                    completion(nil, err ?? .internalServerError)
                    return
                }
            }
        }
    }
}
