// swift-tools-version:4.0
import PackageDescription

let package = Package(
  name: "tfb",
  dependencies: [
    .package(url: "https://github.com/vapor/vapor.git", .branch("gm")),
	// .package(url: "https://github.com/vapor/fluent-postgresql.git", from: "1.0.0-rc"),
    .package(url: "https://github.com/vapor/fluent-mysql.git", from: "3.0.0-rc"),
    // .package(url: "https://github.com/vapor/mongo-provider.git", from: "2.0.0"),
    .package(url: "https://github.com/vapor/leaf.git", from: "3.0.0-rc.1")
  ],
  targets: [
    .target(name: "vapor-tfb-mysql", dependencies: ["FluentMySQL", "Leaf", "TfbCommon"]),
    .target(name: "TfbCommon", dependencies: ["Vapor"]),
    //.target(name: "vapor-tfb-postgresql", dependencies: ["TfbCommon"]),
    //.target(name: "vapor-tfb-mongodb", dependencies: ["TfbCommon"])
  ]
)
