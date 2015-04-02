package main

import (
	"fmt"
	"net/http"
	"encoding/json"
	"flag"

	"github.com/zenazn/goji"
	"github.com/zenazn/goji/web"
)

type Message struct {
	Message string `json:"message"`
}

// Test 1: Json Serialization
func serializeJson(c web.C, w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(&Message{"Hello, World!"})
}

// Test 6: Plaintext
func plaintext(c web.C, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World!")
}

func main() {
	flag.Set("bind", ":8080")
	goji.Get("/json", serializeJson)
	goji.Get("/plaintext", plaintext)
	goji.Serve()
}
