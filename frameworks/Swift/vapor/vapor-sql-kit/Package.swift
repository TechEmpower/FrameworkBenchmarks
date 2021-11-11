// swift-tools-version:5.1
import PackageDescription

let package = Package(
    name: "vapor-sql-kit",
    platforms: [
        .macOS(.v10_15)
    ],
    products: [
        .executable(name: "app", targets: ["App"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "4.0.0"),
        .package(url: "https://github.com/vapor/postgres-kit.git", from: "2.0.0"),
    ],
    targets: [
        .target(name: "App", dependencies: [
            "PostgresKit",
            "Vapor"
        ], path: "Sources")
    ]
)
