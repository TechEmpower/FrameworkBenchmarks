package main

import (
    "context"
    "os"

    "github.com/clubpay/ronykit/kit"
    "github.com/clubpay/ronykit/std/gateways/silverhttp"
)

func main() {
    opts := []kit.Option{
        kit.RegisterService(serviceDesc.Generate()),
        kit.RegisterGateway(
            silverhttp.MustNew(
                silverhttp.Listen(":8080"),
                silverhttp.WithServerName("ronykit"),
            ),
        ),
    }

    for i := range os.Args[1:] {
        switch os.Args[1:][i] {
        case "-prefork":
            opts = append(opts, kit.WithPrefork())
        }
    }

    defer kit.NewServer(opts...).
        Start(context.Background()).
        PrintRoutes(os.Stdout).
        Shutdown(context.Background(), os.Interrupt, os.Kill)
}
