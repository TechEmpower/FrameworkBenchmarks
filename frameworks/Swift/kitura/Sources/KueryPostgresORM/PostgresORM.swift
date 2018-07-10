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

public enum ORMAppError: Error {
    case ORMError(SwiftKueryORM.RequestError)
}

// Configure our ORM Database connection pool as dbConnPool created by KueryPostgres
public func setupORM() {
    Database.default = Database(dbConnPool)
}

/// Get a list of Fortunes from the database.
///
/// - Parameter callback: The callback that will be invoked once the DB query
///                       has completed and results are available, passing an
///                       optional [Fortune] (on success) or ORMAppError on
///                       failure.
///
public func getFortunes(callback: @escaping ([Fortune]?, ORMAppError?) -> Void) -> Void {
    Fortune.findAll { (fortunes, err) in
        if let err = err {
            return callback(nil, .ORMError(err))
        } else {
            callback(fortunes, nil)
        }
    }

}

/// Get a random row (range 1 to 10,000) from the database.
///
/// - Parameter callback: The callback that will be invoked once the DB query
///                       has completed and results are available, passing an
///                       optional RandomRow (on success) or ORMAppError on
///                       failure.
///
public func getRandomRow(callback: @escaping (RandomRow?, ORMAppError?) -> Void) -> Void {
    // Select random row from database range
    let rnd = RandomRow.randomId
    RandomRow.find(id: rnd) { (resultRow, err) in
        if let err = err {
            return callback(nil, .ORMError(err))
        } else {
            callback(resultRow, nil)
        }
    }
}

/// Updates a row of World to a new value.
///
/// - Parameter callback: The callback that will be invoked once the DB update
///                       has completed, passing an optional ORMAppError if the
///                       update failed.
///
public func updateRow(id: Int, callback: @escaping (ORMAppError?) -> Void) -> Void {
    // Generate a random number for this row
    let row = RandomRow(id: id, randomNumber: RandomRow.randomValue)
    row.update(id: id) { (resultRow, err) in
        if let err = err {
            return callback(.ORMError(err))
        } else {
            callback(nil)
        }
    }
}



