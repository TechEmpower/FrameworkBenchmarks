// swift-tools-version:5.10

import PackageDescription

let package = Package(
    name: "vapor-swifql-ikiga",
    platforms: [
       .macOS(.v13)
    ],
    products: [
        .executable(name: "app", targets: ["App"])
    ],
    dependencies: [
        // 💧 A server-side Swift web framework.
        .package(url: "https://github.com/vapor/vapor.git", from: "4.99.3"),
        // 🔵 Non-blocking, event-driven networking for Swift. Used for custom executors
        .package(url: "https://github.com/apple/swift-nio.git", from: "2.65.0"),
        // json encoder/decoder
        .package(url: "https://github.com/orlandos-nl/IkigaJSON.git", from: "2.0.0"),
        // sql builder
        .package(url: "https://github.com/SwifQL/VaporBridges.git", from: "1.0.0-rc"),
        .package(url: "https://github.com/SwifQL/PostgresBridge.git", from: "1.0.0-rc"),
    ],
    targets: [
        .executableTarget(
            name: "App",
            dependencies: [
                .product(name: "Vapor", package: "vapor"),
                .product(name: "NIOCore", package: "swift-nio"),
                .product(name: "NIOPosix", package: "swift-nio"),
                .product(name: "VaporBridges", package: "VaporBridges"),
                .product(name: "PostgresBridge", package: "PostgresBridge"),
                .product(name: "IkigaJSON", package: "IkigaJSON")
            ],
            path: "Sources",
            swiftSettings: swiftSettings
        )
    ]
)

var swiftSettings: [SwiftSetting] { [
    .enableUpcomingFeature("DisableOutwardActorInference"),
    .enableExperimentalFeature("StrictConcurrency"),
] }
