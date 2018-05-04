import Vapor

public let validQueriesRange: ClosedRange<Int> = 1...500

/// Tests 3 and 5 are parameterized with `queries`.
///
/// The queries parameter must be bounded to between 1 and 500.
/// If the parameter is missing, is not an integer, or is an integer less than 1,
/// the value should be interpreted as 1; if greater than 500, the value should be interpreted as 500.
///
/// - Parameter request: HTTP request
/// - Returns: queries
public func queriesParam(for request: Request) throws -> Int {
    // TODO: throw instead of using !
    let rangeMax = try request.parameters.next(String.self)
        .removingPercentEncoding? // convert url-encoded chars
        .components(separatedBy: "...")
        .last? // ignore lower bound, only retain last part which contains upper bound
        .dropLast() // remove ]

    let paramNormalized = rangeMax.flatMap(String.init).flatMap(Int.init) ?? validQueriesRange.upperBound
    return clamp(paramNormalized, to: validQueriesRange)
}

func clamp(_ value: Int, to: ClosedRange<Int>) -> Int {
  return max(to.lowerBound, min(to.upperBound, value))
}
