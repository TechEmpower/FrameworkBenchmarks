// swift-tools-version:5.10
import PackageDescription

let package = Package(
    name: "app",
    platforms: [
        .macOS(.v10_15)
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-nio.git", from: "2.90.0"),
    ],
    targets: [
        .executableTarget(name: "App", dependencies: [
            .product(name: "NIOCore", package: "swift-nio"),
            .product(name: "NIOHTTP1", package: "swift-nio"),
            .product(name: "NIOPosix", package: "swift-nio")
        ], path: "Sources")
    ]
)
