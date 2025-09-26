// swift-tools-version:5.5
import PackageDescription

let package = Package(
    name: "vapor-fluent",
    platforms: [
        .macOS(.v12)
    ],
    products: [
        .executable(name: "app", targets: ["App"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "4.52.2"),
        .package(url: "https://github.com/vapor/fluent.git", from: "4.4.0"),
        .package(url: "https://github.com/vapor/fluent-mongo-driver.git", from: "1.0.2"),
        .package(url: "https://github.com/orlandos-nl/MongoKitten.git", from: "6.7.1")
    ],
    targets: [
        .executableTarget(
            name: "App",
            dependencies: [
                .product(name: "Fluent", package: "fluent"),
                .product(name: "FluentMongoDriver", package: "fluent-mongo-driver"),
                .product(name: "Vapor", package: "vapor"),
                .product(name: "MongoKitten", package: "MongoKitten"),
            ],
            path: "Sources"),
    ]
)