package main

import (
	"encoding/json"
	"net/http"
	"runtime"
)

type MessageStruct struct {
	Message string
}

func hello(w http.ResponseWriter, r *http.Request) {
	m := MessageStruct{"Hello, world"}
	enc := json.NewEncoder(w)
	enc.Encode(m)
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU() * 5) // per mailing list thread results
	http.HandleFunc("/json", hello)
	http.ListenAndServe(":8080", nil)
}
