<img src="https://fenny.github.io/fiber/static/logo.jpg" width="150" alt="Fiber"><br><br>
**[Fiber](https://github.com/fenny/fiber)** is a router framework build on top of **[FastHTTP](https://github.com/valyala/fasthttp)**, the fastest HTTP package for **[Go](https://golang.org/doc/)**.<br>
This library is inspired by **[Express](https://expressjs.com/en/4x/api.html)**, one of the most populair and well known web framework for **[Nodejs](https://nodejs.org/en/about/)**.

### Github Source
* [Fiber](https://github.com/fenny/fiber)

## Version Tested
* [Fiber v0.4.0](https://github.com/fenny/fiber/releases)

## Test URLs
* http://localhost:8080/json
* http://localhost:8080/plaintext

## Benchmark Source
```go
package main

import "github.com/fenny/fiber"

type JSON struct {
	Message string `json:"message"`
}

func main() {
	app := fiber.New()
	app.Settings.Name = "Fiber"
	app.Get("/plaintext", func(c *fiber.Ctx) {
		c.Send("Hello, World!")
	})
	app.Get("/json", func(c *fiber.Ctx) {
		c.Json(JSON{"Hello, World!"})
	})
	app.Listen(8080)
}
```
