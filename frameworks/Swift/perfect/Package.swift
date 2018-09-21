// swift-tools-version:4.0

import PackageDescription

let package = Package(
	name: "Perfect-TechEmpower",
	products: [
		.executable(name: "Perfect", targets: ["Perfect"]),
		.executable(name: "Perfect-MySQL", targets: ["Perfect-MySQL"])
	],
	dependencies: [
		.package(url: "https://github.com/PerfectlySoft/Perfect-HTTPServer.git", from: "3.0.0"),
		.package(url:"https://github.com/PerfectlySoft/Perfect-MySQL.git", from: "3.0.0"),
	],
	targets: [
		.target(name: "Perfect", dependencies: ["PerfectHTTPServer", "PerfectMySQL"]),
		.target(name: "Perfect-MySQL", dependencies: ["PerfectHTTPServer", "PerfectMySQL"])
	]
)
