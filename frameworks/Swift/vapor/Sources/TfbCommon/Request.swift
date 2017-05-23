import Foundation
import Vapor
import HTTP

public let validQueriesRange: ClosedRange<Int> = 1...500

/// Tests 3 and 5 are parameterized with `queries`.
///
/// The queries parameter must be bounded to between 1 and 500.
/// If the parameter is missing, is not an integer, or is an integer less than 1,
/// the value should be interpreted as 1; if greater than 500, the value should be interpreted as 500.
///
/// - Parameter request: HTTP request
/// - Returns: queries
public func queriesParam(for request: Request) -> Int {
    let queriesParam = request.query?["queries"]?.int ?? 1
    return clamp(queriesParam, to: validQueriesRange)
}

func clamp(_ value: Int, to: ClosedRange<Int>) -> Int {
    return max(to.lowerBound, min(to.upperBound, value))
}
