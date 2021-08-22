package main

import (
	"atreugo/src/views"
	"flag"
	"runtime"

	"github.com/savsgio/atreugo/v11"
)

var (
	bindHost string
	prefork  bool
)

func init() { // nolint:gochecknoinits
	flag.StringVar(&bindHost, "bind", ":8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")

	flag.Parse()
}

func main() {
	isNotPreforkOrIsChild := !prefork || atreugo.IsPreforkChild()

	// init database
	if isNotPreforkOrIsChild {
		maxConn := runtime.NumCPU() * 4
		if atreugo.IsPreforkChild() {
			maxConn = 5
		}

		if err := views.InitDB(maxConn); err != nil {
			panic(err)
		}
		defer views.CloseDB()

		// init and populate worlds cache
		views.PopulateWorldsCache()
	}

	// init atreugo server
	server := atreugo.New(atreugo.Config{
		Addr:                          bindHost,
		Name:                          "Go",
		Prefork:                       prefork,
		DisableHeaderNamesNormalizing: true,
	})

	// init views
	if isNotPreforkOrIsChild {
		server.GET("/json", views.JSON)
		server.GET("/db", views.DB)
		server.GET("/queries", views.Queries)
		server.GET("/cached-worlds", views.CachedWorlds)
		server.GET("/fortunes", views.FortunesQuick)
		server.GET("/updates", views.Updates)
		server.GET("/plaintext", views.Plaintext)
	}

	if err := server.ListenAndServe(); err != nil {
		panic(err)
	}
}
