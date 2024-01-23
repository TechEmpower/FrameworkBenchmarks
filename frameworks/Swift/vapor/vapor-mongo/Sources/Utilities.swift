import Vapor

extension Int {
    func bounded(to range: ClosedRange<Int>) -> Int {
        switch self {
        case ...range.lowerBound:
            return range.lowerBound
        case range.upperBound...:
            return range.upperBound
        default:
            return self
        }
    }
}


extension System {
    // tfb-server (aka, citrine) uses 28 hyper-threaded cores
    // postgresql.conf specifies max_connections = 2000
    //
    // 2000 / (28 * 2) = 35.7 (theoretical max)
    //
    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Environment#citrine-self-hosted
    // https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/toolset/databases/postgres/postgresql.conf#L64
    static var maxConnectionsPerEventLoop: Int { 32 }
}