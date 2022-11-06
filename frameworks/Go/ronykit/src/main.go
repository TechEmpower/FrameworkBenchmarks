package main

import (
    "context"
    "os"

    "github.com/clubpay/ronykit/kit"
    "github.com/clubpay/ronykit/std/gateways/fasthttp"
    "github.com/clubpay/ronykit/std/gateways/silverhttp"
)

func main() {

    opts := []kit.Option{
        kit.RegisterService(serviceDesc.Generate()),
    }

    for i := range os.Args[1:] {
        switch os.Args[1:][i] {
        case "-prefork":
            opts = append(opts, kit.WithPrefork())
        case "silverhttp":
            opts = append(opts,
                kit.RegisterGateway(
                    silverhttp.MustNew(
                        silverhttp.Listen(":8080"),
                        silverhttp.WithServerName("ronykit"),
                    ),
                ),
            )

        case "fasthttp":
            opts = append(opts,
                kit.RegisterGateway(
                    fasthttp.MustNew(
                        fasthttp.Listen(":8080"),
                        fasthttp.WithServerName("ronykit"),
                    ),
                ),
            )
        }
    }

    defer kit.NewServer(opts...).
        Start(context.Background()).
        PrintRoutes(os.Stdout).
        Shutdown(context.Background(), os.Interrupt, os.Kill)
}
