package main

import "github.com/fenny/fiber"

// JSON : {"message":"Hello, World!"}
type JSON struct {
	Message string `json:"message"`
}

func main() {
	// New fiber instance
	app := fiber.New()
	// Set Server: Fiber header
	app.Settings.Name = "Fiber"
	// Returns plan text "Hello, World!"
	app.Get("/plaintext", func(c *fiber.Ctx) {
		c.Send("Hello, World!")
	})
	// Returns json {"message":"Hello, World!"}
	app.Get("/json", func(c *fiber.Ctx) {
		c.Json(JSON{"Hello, World!"})
	})
	// Listen on port 8080
	app.Listen(8080)
}
