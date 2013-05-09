package main

import (
	"encoding/json"
	"github.com/hoisie/web"
	"log"
	"os"
	"runtime"
)

type MessageStruct struct {
	Message string
}

func hello(val string) string {
	m := MessageStruct{"Hello, world"}
	j, _ := json.Marshal(m)
	return string(j)
}

func main() {
	f, _ := os.Create("server.log")
	logger := log.New(f, "", log.Ldate|log.Ltime)
	runtime.GOMAXPROCS(runtime.NumCPU())
	web.Get("/(.*)", hello)
	web.SetLogger(logger)
	web.Run("0.0.0.0:8080")
}
