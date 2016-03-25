import Vapor

let app = Application()

app.get("plaintext") { request in
	let response = Response(status: .OK, text: "Hello, World!")
	response.headers["Content-Type"] = "text/plain"
	return response
}

app.get("json") { request in 
	return try Json([
		"message": "Hello, World!"
	])
}

app.start(port: 8080)
