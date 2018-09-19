// swift-tools-version:4.0

import PackageDescription

let package = Package(
	name: "PerfectTemplate",
	products: [
		.executable(name: "PerfectTemplate", targets: ["PerfectTemplate"])
	],
	dependencies: [
		.package(url: "https://github.com/PerfectlySoft/Perfect-HTTPServer.git", from: "3.0.0"),
		// .Package(url:"https://github.com/PerfectlySoft/Perfect-MySQL.git", majorVersion: 3),
	],
	targets: [
		.target(name: "PerfectTemplate", dependencies: ["PerfectHTTPServer"])
	]
)
