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
        .package(url: "https://github.com/mongodb/mongodb-vapor", from: "1.0.0"),
        .package(url: "https://github.com/mongodb/mongo-swift-driver", from: "1.1.0")
    ],
    targets: [
        .executableTarget(
            name: "App",
            dependencies: [
                .product(name: "MongoSwift", package: "mongo-swift-driver"),
                .product(name: "MongoDBVapor", package: "mongodb-vapor"),
                .product(name: "Vapor", package: "vapor"),
            ],
            path: "Sources"),
    ]
)