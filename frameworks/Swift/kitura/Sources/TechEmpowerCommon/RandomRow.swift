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

public struct RandomRow {

    public let id: Int
    public let randomNumber: Int

    public init(id: Int, randomNumber: Int) {
        self.id = id
        self.randomNumber = randomNumber
    }

    public func asDictionary() -> [String: Int] {
        return ["id": self.id, "randomNumber": self.randomNumber]
    }
}
