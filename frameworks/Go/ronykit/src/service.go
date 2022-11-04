package main

import (
    "net/http"

    "github.com/clubpay/ronykit/kit"
    "github.com/clubpay/ronykit/kit/desc"
    "github.com/clubpay/ronykit/std/gateways/fasthttp"
)

var serviceDesc = desc.NewService("RonyKIT_Bench").
        AddContract(
            desc.NewContract().
                Selector(fasthttp.REST(http.MethodGet, "/json")).
                SetInput(kit.RawMessage{}).
                SetOutput(&JSONMessage{}).
                SetHandler(jsonHandler),
        ).
        AddContract(
            desc.NewContract().
                Selector(fasthttp.REST(http.MethodGet, "/plaintext")).
                SetInput(kit.RawMessage{}).
                SetOutput(kit.RawMessage{}).
                SetHandler(plaintextHandler),
        )

func jsonHandler(ctx *kit.Context) {
    ctx.Out().
        SetHdr("Content-Type", "application/json; charset=utf-8").
        SetMsg(&JSONMessage{Message: "Hello, World!"}).
        Send()
}

func plaintextHandler(ctx *kit.Context) {
    ctx.Out().
        SetMsg(kit.RawMessage("Hello, World!")).
        Send()
}
