// swift-tools-version:4.2
import PackageDescription

let package = Package(
    name: "tefb",
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "3.1.0"),
        .package(url: "https://github.com/vapor/postgresql.git", from: "1.0.2"),
    ],
    targets: [
        .target(name: "App", dependencies: ["PostgreSQL", "Vapor"]),
        .target(name: "Run", dependencies: ["App"])
    ]
)

