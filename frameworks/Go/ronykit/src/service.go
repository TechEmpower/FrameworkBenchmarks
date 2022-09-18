package main

import (
    "net/http"

    "github.com/clubpay/ronykit"
    "github.com/clubpay/ronykit/desc"
    "github.com/clubpay/ronykit/std/gateway/fasthttp"
)

var serviceDesc = desc.NewService("RonyKIT_Bench").
        AddContract(
            desc.NewContract().
                Selector(fasthttp.REST(http.MethodGet, "/json")).
                SetInput(&EmptyRequest{}).
                SetOutput(&JSONMessage{}).
                SetHandler(jsonHandler),
        ).
        AddContract(
            desc.NewContract().
                Selector(fasthttp.REST(http.MethodGet, "/plaintext")).
                SetInput(&EmptyRequest{}).
                SetOutput(ronykit.RawMessage{}).
                SetHandler(plaintextHandler),
        )

func jsonHandler(ctx *ronykit.Context) {
    ctx.Out().
        SetHdr("Content-Type", "application/json; charset=utf-8").
        SetMsg(&JSONMessage{Message: "Hello, World!"}).
        Send()
}

func plaintextHandler(ctx *ronykit.Context) {
    ctx.Out().
        SetMsg(ronykit.RawMessage("Hello, World!")).
        Send()
}
