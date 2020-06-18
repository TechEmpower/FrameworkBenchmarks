package main

import (
	"flag"
	"runtime"

	"atreugo/src/views"

	"github.com/savsgio/atreugo/v11"
)

var bindHost string
var prefork bool

func init() {
	flag.StringVar(&bindHost, "bind", ":8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")

	flag.Parse()
}

func numCPU() int {
	n := runtime.NumCPU()
	if n == 0 {
		n = 8
	}

	return n
}

func main() {
	maxConn := numCPU() * 4
	if atreugo.IsPreforkChild() {
		maxConn = numCPU()
	}

	// init database
	if err := views.InitDB(maxConn); err != nil {
		panic(err)
	}
	defer views.CloseDB()

	// init atreugo server
	server := atreugo.New(atreugo.Config{
		Addr:    bindHost,
		Name:    "Go",
		Prefork: prefork,
	})

	// init handlers
	server.GET("/plaintext", views.Plaintext)
	server.GET("/json", views.JSON)
	server.GET("/db", views.DB)
	server.GET("/queries", views.Queries)
	server.GET("/fortune", views.FortuneQuick)
	server.GET("/update", views.Update)

	if err := server.ListenAndServe(); err != nil {
		panic(err)
	}
}
