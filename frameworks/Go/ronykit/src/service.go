package main

import (
    "net/http"
    "sync"

    "github.com/clubpay/ronykit/kit"
    "github.com/clubpay/ronykit/kit/desc"
    "github.com/clubpay/ronykit/kit/utils"
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

var jsonPool = sync.Pool{
    New: func() interface{} {
        return &JSONMessage{}
    },
}

func acquireJSON() *JSONMessage {
    return jsonPool.Get().(*JSONMessage)
}

func releaseJSON(m *JSONMessage) {
    jsonPool.Put(m)
}

func jsonHandler(ctx *kit.Context) {
    jsonMsg := acquireJSON()
    jsonMsg.Message = "Hello, World!"

    ctx.Out().
        SetHdr("Content-Type", "application/json; charset=utf-8").
        SetMsg(jsonMsg).
        Send()
    releaseJSON(jsonMsg)
}

func plaintextHandler(ctx *kit.Context) {
    ctx.Out().
        SetHdr("Content-Type", "text/plain").
        SetMsg(kit.RawMessage(utils.S2B("Hello, World!"))).
        Send()
}
