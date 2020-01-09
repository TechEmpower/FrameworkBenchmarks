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
