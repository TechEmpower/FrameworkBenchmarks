package main

import (
	"fasthttp/src/handlers"
	"flag"
	"runtime"

	"github.com/savsgio/gotils/strconv"
	"github.com/valyala/fasthttp"
	fastprefork "github.com/valyala/fasthttp/prefork"
)

var bindHost string

var prefork bool

func init() {
	flag.StringVar(&bindHost, "bind", ":8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")

	flag.Parse()
}

func main() {
	isNotPreforkOrIsChild := !prefork || fastprefork.IsChild()
	handler := func(ctx *fasthttp.RequestCtx) {}

	if isNotPreforkOrIsChild {
		maxConn := runtime.NumCPU()
		if fastprefork.IsChild() {
			maxConn = 5
		}

		// init database
		if err := handlers.InitDB(maxConn); err != nil {
			panic(err)
		}
		defer handlers.CloseDB()

		// init and populate worlds cache
		handlers.PopulateWorldsCache()

		// init handler
		handler = func(ctx *fasthttp.RequestCtx) {
			switch strconv.B2S(ctx.Path()) {
			case "/json":
				handlers.JSON(ctx)
			case "/db":
				handlers.DB(ctx)
			case "/queries":
				handlers.Queries(ctx)
			case "/cached-worlds":
				handlers.CachedWorlds(ctx)
			case "/fortunes":
				handlers.FortunesQuick(ctx)
			case "/updates":
				handlers.Updates(ctx)
			case "/plaintext":
				handlers.Plaintext(ctx)
			default:
				ctx.Error(fasthttp.StatusMessage(fasthttp.StatusNotFound), fasthttp.StatusNotFound)
			}
		}
	}

	// init fasthttp server
	server := &fasthttp.Server{
		Name:                          "Go",
		Handler:                       handler,
		DisableHeaderNamesNormalizing: true,
	}

	listenAndServe := server.ListenAndServe
	if prefork {
		listenAndServe = fastprefork.New(server).ListenAndServe
	}

	if err := listenAndServe(bindHost); err != nil {
		panic(err)
	}
}
