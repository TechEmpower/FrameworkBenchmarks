package main

import (
	"flag"
	"log"

	"github.com/savsgio/atreugo/v7"
)

func main() {
	// init flags
	bindPort := flag.Int("bind", 8080, "set bind port")
	// prefork := flag.Bool("prefork", false, "use prefork")
	// easyjson := flag.Bool("easyjson", false, "use easyjson")
	// child := flag.Bool("child", false, "is child proc")
	// dbDriver := flag.String("db", "none", "db connection driver [values: pq || pgx || mysql || mgo || none]")
	// dbConnectionString := flag.String("db_connection_string",
	// 	"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
	// 	"db connection string")
	flag.Parse()

	server := atreugo.New(&atreugo.Config{
		Host:     "0.0.0.0",
		Port:     *bindPort,
		Compress: false,
	})

	server.Path("GET", "/plaintext", func(ctx *atreugo.RequestCtx) error {
		ctx.SetContentType("text/plain")
		_, err := ctx.WriteString("Hello, World!")
		return err
	})

	server.Path("GET", "/json", func(ctx *atreugo.RequestCtx) error {
		return ctx.JSONResponse(atreugo.JSON{"Message": "Hello, World!"})
	})

	log.Fatal(server.ListenAndServe())
}
