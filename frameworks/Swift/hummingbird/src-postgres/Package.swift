// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "server",
    platforms: [.macOS(.v10_15)],
    products: [
        .executable(name: "server", targets: ["server"])
    ],
    dependencies: [
        .package(url: "https://github.com/hummingbird-project/hummingbird.git", .upToNextMinor(from: "0.13.1")),
        .package(url: "https://github.com/hummingbird-project/hummingbird-mustache.git", from: "1.0.1"),
        .package(url: "https://github.com/vapor/postgres-kit.git", from: "2.3.0"),
    ],
    targets: [
        .target(name: "server",
            dependencies: [
                .product(name: "Hummingbird", package: "hummingbird"),
                .product(name: "HummingbirdFoundation", package: "hummingbird"),
                .product(name: "HummingbirdMustache", package: "hummingbird-mustache"),
                .product(name: "PostgresKit", package: "postgres-kit"),
            ],
            swiftSettings: [
                // Enable better optimizations when building in Release configuration. Despite the use of
                // the `.unsafeFlags` construct required by SwiftPM, this flag is recommended for Release
                // builds. See <https://github.com/swift-server/guides#building-for-production> for details.
                .unsafeFlags(["-cross-module-optimization"], .when(configuration: .release))
            ]
        ),
    ]
)
