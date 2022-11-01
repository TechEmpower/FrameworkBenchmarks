package main

import (
    "context"
    "os"

    "github.com/clubpay/ronykit"
    "github.com/clubpay/ronykit/std/gateway/fasthttp"
)

func main() {
    defer ronykit.NewServer(
        ronykit.RegisterBundle(
            fasthttp.MustNew(
                fasthttp.Listen(":8080"),
                fasthttp.WithServerName("ronykit"),
            ),
        ),
        ronykit.RegisterService(serviceDesc.Generate()),
    ).
        Start(context.Background()).
        PrintRoutes(os.Stdout).
        Shutdown(context.Background(), os.Interrupt, os.Kill)
}
