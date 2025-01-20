package main

import (
	"log"

	"github.com/BryanMwangi/pine"
)

func plaintextHandler(c *pine.Ctx) error {
	c.Set("Server", "Pine")
	return c.SendString("Hello, World!")
}

func jsonHandler(c *pine.Ctx) error {
	c.Set("Server", "Pine")
	return c.JSON(map[string]string{
		"message": "Hello, World!",
	})
}

func main() {
	app := pine.New()
	app.Get("/plaintext", plaintextHandler)
	app.Get("/json", jsonHandler)

	// Start the server on port 3000
	log.Fatal(app.Start(":8080"))
}
