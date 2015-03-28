package main

import (
	"fmt"
	"net/http"
	"flag"

	"github.com/zenazn/goji"
	"github.com/zenazn/goji/web"
)

func plaintext(c web.C, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World!")
}

func main() {
	flag.Set("bind", ":8080")
	goji.Get("/plaintext", plaintext)
	goji.Serve()
}
