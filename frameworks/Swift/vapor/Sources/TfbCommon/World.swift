import Random

public struct WorldMeta {
  private init() { }

  public static let maxId: UInt32 = 10000
  public static let randomId = { () -> UInt32 in UInt32(1) + CryptoRandom.uint32 % maxId }
  public static let maxRandomNumber: Int32 = 10000
  public static let randomRandomNumber = { () -> Int32 in Int32(1) + abs(CryptoRandom.int32) % maxRandomNumber }
}
