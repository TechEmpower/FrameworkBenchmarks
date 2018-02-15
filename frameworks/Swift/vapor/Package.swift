import PackageDescription

let package = Package(
  name: "tfb",
  targets: [
    Target(name: "vapor-tfb-mysql", dependencies: ["TfbCommon"]),
    Target(name: "vapor-tfb-postgresql", dependencies: ["TfbCommon"]),
    Target(name: "vapor-tfb-mongodb", dependencies: ["TfbCommon"])
  ],
  dependencies: [
    .Package(url: "https://github.com/vapor/vapor.git", "2.4.4"),
    .Package(url: "https://github.com/vapor/postgresql-provider.git", majorVersion:2),
    .Package(url: "https://github.com/vapor/mysql-provider.git", majorVersion: 2),
    .Package(url: "https://github.com/vapor/mongo-provider.git", majorVersion: 2),
    .Package(url: "https://github.com/vapor/leaf-provider.git", majorVersion: 1)
  ],
  exclude: [
    "Config",
    "Database",
    "Localization",
    "Public",
    "Resources",
  ]
)
