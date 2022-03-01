package main

import (
	"net/http"
	"runtime"

	"github.com/gogf/gf/v2/frame/g"
	"goframe/handler"
)

const (
	routeJson         = `/json`
	routeDb           = `/db`
	routeQueries      = `/queries`
	routeCachedWorlds = `/cached-worlds`
	routeFortunes     = `/fortunes`
	routeUpdates      = `/updates`
	routePlaintext    = `/plaintext`
)

func main() {
	// Db settings.
	if err := handler.InitDB(runtime.NumCPU() * 4); err != nil {
		panic(err)
	}
	defer handler.CloseDB()

	// Init and populate worlds cache.
	handler.PopulateWorldsCache()

	// Init http server and handler.
	s := g.Server()
	s.SetHandler(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Server", "GoFrame")
		switch r.URL.Path {
		case routeJson:
			handler.JSON(w, r)
		case routeDb:
			handler.DB(w, r)
		case routeQueries:
			handler.Queries(w, r)
		case routeCachedWorlds:
			handler.CachedWorlds(w, r)
		case routeFortunes:
			handler.FortunesQuick(w, r)
		case routeUpdates:
			handler.Updates(w, r)
		case routePlaintext:
			handler.Plaintext(w, r)
		default:
			w.WriteHeader(http.StatusNotFound)
			_, _ = w.Write([]byte(http.StatusText(http.StatusNotFound)))
		}
	})
	s.SetPort(8080)
	s.Run()
}
