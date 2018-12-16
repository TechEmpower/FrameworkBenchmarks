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
import Configuration
import MongoKitten
import TechEmpowerCommon

#if os(Linux)
    import Glibc
#else
    import Darwin
#endif

public enum MongoAppError: Error {
    case MongoError(String)
}

// We will load our database configuration from config.json, but this can be
// overridden with the TFB_DB_CONFIG environment variable.
let configurationFilename: String = ProcessInfo.processInfo.environment["TFB_DB_CONFIG"] ?? "config.json"
let manager = ConfigurationManager().load(file: configurationFilename, relativeFrom: .pwd).load(.environmentVariables)

let dbHost = manager["DB_HOST"] as? String ?? manager["mongodb:host"] as? String ?? "localhost"
let dbPort = Int32(manager["DB_PORT"] as? String != nil ? Int(manager["DB_PORT"] as! String) ?? 27017 : manager["mongodb:port"] as? Int ?? 27017)
let dbName = manager["mongodb:name"] as? String ?? "hello_world"
let dbUser = manager["mongodb:user"] as? String ?? "benchmarkdbuser"
let dbPass = manager["mongodb:password"] as? String ?? "benchmarkdbpass"

let dbRows = 10000
let maxValue = 10000

var myDatabase: MongoKitten.Database?
var world: MongoKitten.Collection?
var fortune: MongoKitten.Collection?

func connectToDB() throws {
    let connectString = "mongodb://\(dbHost):\(dbPort)/\(dbName)"
    Log.info("Connect string = \(connectString)")
    //myDatabase = try MongoKitten.Database("mongodb://\(dbUser):\(dbPass)@\(dbHost):\(dbPort)/\(dbName)")
    myDatabase = try MongoKitten.Database(connectString)
    guard let myDatabase = myDatabase else {
        throw AppError.ConnectionError("Nil MongoDB connection to \(connectString)")
    }
    guard myDatabase.server.isConnected else {
        throw AppError.ConnectionError("Not connected to \(connectString)")
    }
    world = myDatabase["world"]
    fortune = myDatabase["fortune"]
}

// Allow construction of a Fortune from a MongoKitten Document
extension Fortune {
    init(document: Document) throws {
        if let id = Int(document["_id"]), let message = String(document["message"]) {
            self.init(id: id, message: message)
        } else {
            throw AppError.DataFormatError("Expected fields of Fortune document could not be retreived")
        }
    }
}

func getFortunes() throws -> [Fortune] {
    guard let fortune = fortune else {
        throw MongoAppError.MongoError("Fortune collection not initialized")
    }
    
//    let allFortunes: [Document] = Array(try fortune.find())
    let allFortunes = try fortune.find()
    let resultFortunes: [Fortune] = try allFortunes.map { try Fortune.init(document: $0) }
    return resultFortunes
}

// Get a random row (range 1 to 10,000) from DB: id(int),randomNumber(int)
// Convert to object using object-relational mapping (ORM) tool
// Serialize object to JSON - example: {"id":3217,"randomNumber":2149}
func getRandomRow() throws -> [String:Int] {
    guard let world = world else {
        throw MongoAppError.MongoError("World collection not initialized")
    }

    let rnd = RandomRow.randomId
    let result = try world.findOne("_id" == rnd)
    guard let document = result else {
        throw AppError.DataFormatError("World entry id=\(rnd) not found")
    }
    guard let id = Int(document["_id"]), let randomNumber = Int(document["randomNumber"]) else {
        throw AppError.DataFormatError("Expected fields of World document could not be retreived")
    }
    return ["id":id, "randomNumber":Int(randomNumber)]
}

// Updates a row of World to a new value.
func updateRandomRow() throws -> [String:Int] {
    guard let world = world else {
        throw MongoAppError.MongoError("World collection not initialized")
    }
    
    let rnd = RandomRow.randomId
    let rndValue = RandomRow.randomValue
    let document = try world.findAndUpdate("_id" == rnd, with: ["randomNumber": rndValue])
    guard let id = Int(document["_id"]), let randomNumber = Int(document["randomNumber"]) else {
        throw AppError.DataFormatError("Expected fields of World document could not be retreived")
    }
    return ["id":id, "randomNumber":Int(randomNumber)]
}



