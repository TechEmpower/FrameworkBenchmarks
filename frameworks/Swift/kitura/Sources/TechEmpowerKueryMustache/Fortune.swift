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

struct Fortune {

  let id: Int
  let message: String

  public init(id: Int, message: String) {
    self.id = id
    self.message = message
  }

  init(row: [String:Any?]) throws {
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

  static func == (lhs: Fortune, rhs: Fortune) -> Bool {
    return lhs.id == rhs.id && lhs.message == rhs.message
  }

  static func < (lhs: Fortune, rhs: Fortune) -> Bool {
    return lhs.message < rhs.message || (lhs.message == rhs.message && lhs.id < rhs.id)
  }

}
