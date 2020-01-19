// swift-tools-version:5.1
import PackageDescription

let package = Package(
    name: "app",
    platforms: [
        .macOS(.v10_15)
    ],
    products: [
        .executable(name: "app", targets: ["App"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "4.0.0-beta"),
        .package(url: "https://github.com/vapor/fluent.git", from: "4.0.0-beta"),
        .package(url: "https://github.com/vapor/fluent-postgres-driver.git", from: "2.0.0-beta"),
    ],
    targets: [
        .target(name: "App", dependencies: [
            "Fluent",
            "FluentPostgresDriver",
            "Vapor"
        ], path: "Sources")
    ]
)
