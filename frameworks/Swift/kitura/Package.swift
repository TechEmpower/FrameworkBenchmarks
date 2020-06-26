// swift-tools-version:4.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "Kitura-TechEmpower",
    dependencies: [
        .package(url: "https://github.com/IBM-Swift/Kitura.git", .upToNextMinor(from: "2.6.0")),
        .package(url: "https://github.com/IBM-Swift/LoggerAPI.git", from: "1.8.0"),
        .package(url: "https://github.com/IBM-Swift/HeliumLogger.git", from: "1.8.0"),
        .package(url: "https://github.com/IBM-Swift/Configuration.git", from: "3.0.0"),
        .package(url: "https://github.com/IBM-Swift/Swift-Kuery-PostgreSQL.git", from: "2.0.0"),
        .package(url: "https://github.com/IBM-Swift/Swift-Kuery-ORM.git", from: "0.4.0"),
        .package(url: "https://github.com/IBM-Swift/Kitura-StencilTemplateEngine.git", from: "1.9.0"),
        .package(url: "https://github.com/IBM-Swift/Kitura-MustacheTemplateEngine.git", from: "1.7.2"),
        .package(url: "https://github.com/OpenKitten/MongoKitten.git", from: "4.1.3"),
    ],
    targets: [
        .target(
            name: "TechEmpowerCommon",
            dependencies: []),
        .target(
            name: "KueryPostgres",
            dependencies: [.target(name: "TechEmpowerCommon"), "Configuration", "SwiftKueryPostgreSQL"]),
        .target(
            name: "KueryPostgresRaw",
            dependencies: [.target(name: "KueryPostgres"), "LoggerAPI"]),
        .target(
            name: "KueryPostgresORM",
            dependencies: [.target(name: "KueryPostgres"), "LoggerAPI", "SwiftKueryORM"]),
        .target(
            name: "TechEmpower",
            dependencies: ["Kitura"]),
        .target(
            name: "TechEmpowerPostgres",
            dependencies: [.target(name: "KueryPostgresRaw"), "Kitura", "HeliumLogger", "KituraStencil"]),
        .target(
            name: "TechEmpowerPostgresORM",
            dependencies: [.target(name: "KueryPostgresORM"), "Kitura", "HeliumLogger", "KituraStencil"]),
        .target(
            name: "TechEmpowerPostgresORMCodable",
            dependencies: [.target(name: "KueryPostgresORM"), "Kitura", "HeliumLogger", "KituraStencil"]),
        .target(
            name: "TechEmpowerPostgresMustache",
            dependencies: [.target(name: "KueryPostgresRaw"), "Kitura", "HeliumLogger", "KituraMustache"]),
        .target(
            name: "TechEmpowerMongoKitten",
            dependencies: [.target(name: "TechEmpowerCommon"), "Kitura", "HeliumLogger", "Configuration", "MongoKitten", "KituraStencil"]),
    ]
)
