// swift-tools-version:4.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "SwiftNIOTFB",
    dependencies: [
        .package(url: "https://github.com/apple/swift-nio.git", from: "1.3.1"),
    ],
    targets: [
        .target(name: "swift-nio-tfb-default", dependencies: ["NIO", "NIOHTTP1"]),
    ]
)
