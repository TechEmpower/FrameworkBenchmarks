package main

import (
	"encoding/json"
	"github.com/hoisie/web"
	"io/ioutil"
	"log"
	"runtime"
)

type MessageStruct struct {
	Message string `json:"message"`
}

func hello(ctx *web.Context, val string) {
	m := MessageStruct{"Hello, World!"}
	j, _ := json.Marshal(m)
	ctx.ContentType("application/json")
	ctx.Write(j)
}

func main() {
	logger := log.New(ioutil.Discard, "", 0)
	runtime.GOMAXPROCS(runtime.NumCPU())
	web.Get("/(.*)", hello)
	web.SetLogger(logger)
	web.Run("0.0.0.0:8080")
}
