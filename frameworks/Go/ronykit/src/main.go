package main

import (
    "context"
    "os"

    "github.com/clubpay/ronykit/kit"
    "github.com/clubpay/ronykit/std/gateways/fasthttp"
)

func main() {
    defer kit.NewServer(
        kit.RegisterGateway(
            fasthttp.MustNew(
                fasthttp.Listen(":8080"),
                fasthttp.WithServerName("ronykit"),
            ),
        ),
        kit.RegisterService(serviceDesc.Generate()),
    ).
        Start(context.Background()).
        PrintRoutes(os.Stdout).
        Shutdown(context.Background(), os.Interrupt, os.Kill)
}
