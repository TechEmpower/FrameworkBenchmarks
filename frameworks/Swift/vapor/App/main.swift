import Vapor

let app = Application()

app.get("plaintext") { request in
	var response = Response(status: .ok, text: "Hello, World!")
	response.headers["Content-Type"] = "text/plain"
	return response
}

app.get("json") { request in 
	return Json([
		"message": "Hello, World!"
	])
}

app.start(port: 8080)
