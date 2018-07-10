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

public struct Fortune: Codable {

    /// The id of this Fortune
    public let id: Int

    /// The message contained within this Fortune
    public let message: String

    public init(id: Int, message: String) {
        self.id = id
        self.message = message
    }

    /// Create a Fortune instance from a [String: Any?] dictionary,
    /// such as that retrieved by Kuery.
    ///
    /// - Parameter row: A dictionary representing the fields of a
    ///                  Fortune database row.
    /// - throws: if the fields and types contained in the dictionary
    ///           do not match those expected.
    public init(row: [String:Any?]) throws {
        guard let idField = row["id"] else {
            throw AppError.DataFormatError("Missing 'id' field")
        }
        guard let msgField = row["message"] else {
            throw AppError.DataFormatError("Missing 'message' field")
        }
        guard let message = msgField as? String else {
            throw AppError.DataFormatError("'message' field not a String")
        }
        guard let id = idField as? Int32 else {
            throw AppError.DataFormatError("'id' field not an Int32")
        }
        self.init(id: Int(id), message: message)
    }

}

extension Fortune: Comparable {

    public static func == (lhs: Fortune, rhs: Fortune) -> Bool {
        return lhs.id == rhs.id && lhs.message == rhs.message
    }

    public static func < (lhs: Fortune, rhs: Fortune) -> Bool {
        return lhs.message < rhs.message || (lhs.message == rhs.message && lhs.id < rhs.id)
    }

}
