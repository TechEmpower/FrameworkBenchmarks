// swift-tools-version:4.0

import PackageDescription

let package = Package(
	name: "Perfect-TechEmpower",
	products: [
		.executable(name: "Perfect", targets: ["Perfect"]),
		.executable(name: "Perfect-MySQL", targets: ["Perfect-MySQL"]),
		.executable(name: "Perfect-PostgreSQL", targets: ["Perfect-PostgreSQL"]),
		.executable(name: "Perfect-MongoDB", targets: ["Perfect-MongoDB"])
	],
	dependencies: [
		.package(url: "https://github.com/PerfectlySoft/Perfect-HTTPServer.git", from: "3.0.0"),
		.package(url:"https://github.com/PerfectlySoft/Perfect-MySQL.git", from: "3.0.0"),
		.package(url: "https://github.com/PerfectlySoft/Perfect-PostgreSQL.git", from: "3.0.0"),
		.package(url:"https://github.com/PerfectlySoft/Perfect-MongoDB.git", from: "3.0.0")
	],
	targets: [
		.target(name: "Perfect", dependencies: ["PerfectHTTPServer"]),
		.target(name: "Perfect-MySQL", dependencies: ["PerfectHTTPServer", "PerfectMySQL"]),
		.target(name: "Perfect-PostgreSQL", dependencies: ["PerfectHTTPServer", "PerfectPostgreSQL"]),
		.target(name: "Perfect-MongoDB", dependencies: ["PerfectHTTPServer", "PerfectMongoDB"])
	]
)
