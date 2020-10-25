// swift-tools-version:5.1
import PackageDescription

let package = Package(
    name: "vapor-default",
    platforms: [
        .macOS(.v10_15)
    ],
    products: [
        .executable(name: "app", targets: ["App"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "4.0.0"),
    ],
    targets: [
        .target(name: "App", dependencies: [
            "Vapor",
        ], path: "Sources")
    ]
)
