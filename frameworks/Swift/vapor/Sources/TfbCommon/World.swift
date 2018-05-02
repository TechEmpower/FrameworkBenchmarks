import Vapor
#if os(Linux)
import Glibc
#endif

internal extension Int {
    static func random<T>(_ max: T) -> Int where T: BinaryInteger {
        #if os(Linux)
        return Int(SwiftGlibc.random() % (max + 1))
        #else
        return Int(arc4random_uniform(UInt32(max)))
        #endif
    }
}

public struct WorldMeta {
    private init() { }

    public static let maxId: UInt32 = 10_000

    public static func randomId() -> Int {
        return Int.random(maxId)
    }

    public static let maxRandomNumber: Int32 = 10_000
    public static func randomRandomNumber() -> Int {
        return Int.random(maxRandomNumber)
    }
}
