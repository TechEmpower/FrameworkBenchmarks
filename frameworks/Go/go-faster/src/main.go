package main

import (
	"context"
	"flag"
	"log/slog"
	"net/http"

	"github.com/go-faster/hx"
)

//go:generate go tool jschemagen -target message.go -typename Message message.schema.json

func main() {
	var arg struct {
		Workers int
		Addr    string
		Mode    string
	}
	flag.StringVar(&arg.Addr, "addr", ":8080", "listen address")
	flag.IntVar(&arg.Workers, "j", 1024, "count of workers")
	flag.Parse()

	slog.Info("starting server",
		slog.String("addr", arg.Addr),
		slog.Int("workers", arg.Workers),
	)

	s := &hx.Server{
		Workers: arg.Workers,
		Name:    "hx",
		Handler: func(ctx *hx.Ctx) {
			switch string(ctx.Request.URI().Path()) {
			case "/plaintext":
				ctx.Response.Header.Add("Content-Type", "text/plain")
				ctx.Response.AppendBodyString("Hello, World!")
			case "/json":
				ctx.Response.Header.Add("Content-Type", "application/json")
				msg := Message{Message: "Hello, World!"}
				ctx.JSON.Encode(msg)
				ctx.Response.AppendBody(ctx.JSON.Encoder.Bytes())
			default:
				ctx.Response.SetStatusCode(http.StatusNotFound)
			}
		},
	}
	if err := s.ListenAndServe(context.Background(), arg.Addr); err != nil {
		slog.Error("Failed to start server", slog.Any("err", err))
	}
}
