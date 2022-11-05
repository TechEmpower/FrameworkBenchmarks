package main

import (
    "context"
    "os"

    "github.com/clubpay/ronykit/kit"
    "github.com/clubpay/ronykit/std/gateways/fasthttp"
)

func main() {

    opts := []kit.Option{
        kit.RegisterGateway(
            fasthttp.MustNew(
                fasthttp.Listen(":8080"),
                fasthttp.WithServerName("ronykit"),
            ),
        ),
        kit.RegisterService(serviceDesc.Generate()),
    }

    for i := range os.Args[1:] {
        if os.Args[1:][i] == "-prefork" {
            opts = append(opts, kit.WithPrefork())
        }
    }

    defer kit.NewServer(opts...).
        Start(context.Background()).
        PrintRoutes(os.Stdout).
        Shutdown(context.Background(), os.Interrupt, os.Kill)
}
