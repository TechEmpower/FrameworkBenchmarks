// swift-tools-version:4.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "Kitura-TechEmpower",
    dependencies: [
        .package(url: "https://github.com/IBM-Swift/Kitura.git", from: "2.3.0"),
        .package(url: "https://github.com/IBM-Swift/HeliumLogger.git", from: "1.7.0"),
        .package(url: "https://github.com/IBM-Swift/Configuration.git", from: "3.0.0"),
        .package(url: "https://github.com/IBM-Swift/Swift-Kuery-PostgreSQL", from: "1.0.0"),
        .package(url: "https://github.com/IBM-Swift/Kitura-StencilTemplateEngine.git", from: "1.9.0"),
        .package(url: "https://github.com/IBM-Swift/Kitura-MustacheTemplateEngine.git", from: "1.7.2"),
        .package(url: "https://github.com/OpenKitten/MongoKitten.git", from: "4.1.3"),
    ],
    targets: [
        .target(
            name: "TechEmpowerKuery",
            dependencies: ["Kitura", "HeliumLogger", "Configuration", "SwiftKueryPostgreSQL", "KituraStencil"]),
        .target(
            name: "TechEmpowerKueryMustache",
            dependencies: ["Kitura", "HeliumLogger", "Configuration", "SwiftKueryPostgreSQL", "KituraMustache"]),
        .target(
            name: "TechEmpowerMongoKitten",
            dependencies: ["Kitura", "HeliumLogger", "Configuration", "MongoKitten", "KituraStencil"]),
    ]
)
