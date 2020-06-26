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

// Return a random number within the range of rows in the database
private func randomNumberGenerator(_ maxVal: Int) -> Int {
    #if os(Linux)
    return Int(random() % maxVal) + 1
    #else
    return Int(arc4random_uniform(UInt32(maxVal))) + 1
    #endif
}

public struct RandomRow: Codable {

    /// The number of rows in the World table
    public static let dbRows = 10000

    /// The maximum value for randomNumber
    public static let maxValue = 10000

    /// A generated random row id suitable for retrieving
    /// or creating a RandomRow instance.
    public static var randomId: Int {
        return randomNumberGenerator(dbRows)
    }

    /// A generated random value suitable for assigning as the
    /// `randomNumber` for a RandomRow instance.
    public static var randomValue: Int {
        return randomNumberGenerator(maxValue)
    }

    /// The id for this RandomRow, ranging from 1 to dbRows
    public let id: Int

    /// A random number ranging from 1 to maxValue
    public let randomNumber: Int

    public init(id: Int, randomNumber: Int) {
        self.id = id
        self.randomNumber = randomNumber
    }

    /// Map the properties of this type to their corresponding database
    /// column names (required by the ORM).
    enum CodingKeys: String, CodingKey {
        case id
        case randomNumber = "randomnumber"
    }

    /// Returns a JSON-convertible dictionary representation of this RandomRow.
    public func asDictionary() -> [String: Int] {
        return ["id": self.id, "randomNumber": self.randomNumber]
    }
}
