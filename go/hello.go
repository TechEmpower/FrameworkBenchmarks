package main

import (
	"encoding/json"
	"net/http"
	"runtime"
	"strconv"
)

type MessageStruct struct {
	Message string
}

func hello(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(&MessageStruct{"Hello, world"})
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	http.HandleFunc("/json", hello)
	http.ListenAndServe(":8080", nil)
}
