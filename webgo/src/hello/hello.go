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

func hello(val string) string {
	m := MessageStruct{"Hello, World!"}
	j, _ := json.Marshal(m)
	return string(j)
}

func main() {
	logger := log.New(ioutil.Discard, "", 0)
	runtime.GOMAXPROCS(runtime.NumCPU())
	web.Get("/(.*)", hello)
	web.SetLogger(logger)
	web.Run("0.0.0.0:8080")
}
