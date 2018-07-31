// swift-tools-version:4.0
import PackageDescription

let package = Package(
  name: "tfb",
  dependencies: [
    .package(url: "https://github.com/vapor/vapor.git", from: "3.0.0"),
    .package(url: "https://github.com/vapor/fluent-mysql.git", from: "3.0.0-rc"),
    .package(url: "https://github.com/vapor/leaf.git", from: "3.0.0-rc.1")
  ],
  targets: [
    .target(name: "vapor-tfb", dependencies: ["Vapor"], exclude: ["Resources"]),
    .target(name: "vapor-tfb-mysql", dependencies: ["FluentMySQL", "Leaf", "Vapor"]),
  ]
)
