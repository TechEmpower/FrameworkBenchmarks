package main

import (
	"github.com/apex/log"
	"github.com/gramework/gramework"
)

func main() {
	gramework.Logger.Level = log.FatalLevel
	app := gramework.New()

	app.GET("/plaintext", func(ctx *gramework.Context) error {
		_, err := ctx.WriteString("Hello, World!")
		return err
	})

	app.GET("/json", func(ctx *gramework.Context) error {
		ctx.SetContentType("application/json")
		b, err := ctx.ToJSON(Message{Message: "Hello, World!"})
		if err != nil {
			return err
		}
		_, err = ctx.Write(b)
		return err
	})

	app.ListenAndServe(":8080")
}
