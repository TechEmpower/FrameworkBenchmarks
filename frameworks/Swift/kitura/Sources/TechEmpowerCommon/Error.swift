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

/// Represents various errors that can occur with the Kitura TechEmpower benchmark.
public enum AppError: Error {

    /// An error occurring when executing a raw SQL query against a database.
    case DBError(String, query: String)

    /// An error occurring when executing a Kuery operation against a database.
    case DBKueryError(String)

    /// An error occurring when the format of the data retrieved by a database
    /// operation was not as expected.
    case DataFormatError(String)

    /// An error occurring when a connection to the database cannot be established.
    case ConnectionError(String)

    /// Any other type of error
    case OtherError(String)
}
