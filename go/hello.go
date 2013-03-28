package main

import (
  "net/http";
  "encoding/json";
  "runtime";
  "fmt";
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
  runtime.GOMAXPROCS(runtime.NumCPU())
  http.HandleFunc("/json", hello)
  http.ListenAndServe(":8080", nil)
}
