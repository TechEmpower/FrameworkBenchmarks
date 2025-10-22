package main

import (
	"io/ioutil"
	"log"
	"net/http"
	"runtime"

	"github.com/JaCoB1123/web"
	"github.com/tidwall/sjson"
)

type MessageStruct struct {
	Message string `json:"message"`
}

func getJSON(ctx *web.Context) []byte {
	ctx.SetHeader("Server", "web.go", false)
	ctx.SetHeader("Content-Type", "application/json", true)
	j, _ := sjson.SetBytes(nil, "message", "Hello, World!")
	return j
}

func main() {
	logger := log.New(ioutil.Discard, "", 0)
	runtime.GOMAXPROCS(runtime.NumCPU())

	server := web.NewServer()
	server.Get("/json", getJSON)
	server.SetLogger(logger)

	http.ListenAndServe(":8080", server)
}
