package main

import (
	"flag"
	"fmt"
	"log"
	"runtime"

	"fasthttp/src/handlers"
	"fasthttp/src/storage"

	"github.com/savsgio/gotils"
	"github.com/valyala/fasthttp"
	fastprefork "github.com/valyala/fasthttp/prefork"
)

var bindHost, jsonEncoder, dbDriver, dbConnectionString string
var prefork bool

func init() {
	// init flags
	flag.StringVar(&bindHost, "bind", ":8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")
	flag.StringVar(&jsonEncoder, "json_encoder", "none", "json encoder: none or easyjson or sjson")
	flag.StringVar(&dbDriver, "db", "none", "db connection driver [values: none or pgx or mongo]")
	flag.StringVar(&dbConnectionString, "db_connection_string", "", "db connection string")

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
	if fastprefork.IsChild() {
		maxConn = numCPU()
	}

	if dbConnectionString == "" {
		dbConnectionString = fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d", "tfb-database", 5432, "benchmarkdbuser", "benchmarkdbpass", "hello_world", maxConn)
	}

	// init database with appropriate driver
	db, err := storage.InitDB(dbDriver, dbConnectionString, maxConn)
	if err != nil {
		log.Fatal(err)
	}

	if db != nil {
		defer db.Close()
	}

	// init json encoders
	var jsonHandler, dbHandler, queriesHandler, updateHandler fasthttp.RequestHandler

	switch jsonEncoder {
	case "easyjson":
		jsonHandler = handlers.JSONHandlerEasyJSON
		dbHandler = handlers.DBHandlerEasyJSON(db)
		queriesHandler = handlers.QueriesHandlerEasyJSON(db)
		updateHandler = handlers.UpdateHandlerEasyJSON(db)
	case "sjson":
		jsonHandler = handlers.JSONHandlerSJson
		dbHandler = handlers.DBHandlerSJson(db)
		queriesHandler = handlers.QueriesHandlerSJson(db)
		updateHandler = handlers.UpdateHandlerSJson(db)
	default:
		jsonHandler = handlers.JSONHandler
		dbHandler = handlers.DBHandler(db)
		queriesHandler = handlers.QueriesHandler(db)
		updateHandler = handlers.UpdateHandler(db)
	}

	fortunesHandler := handlers.FortuneHandler(db)
	fortunesQuickHandler := handlers.FortuneQuickHandler(db)

	handler := func(ctx *fasthttp.RequestCtx) {
		switch gotils.B2S(ctx.Path()) {
		case "/plaintext":
			handlers.PlaintextHandler(ctx)
		case "/json":
			jsonHandler(ctx)
		case "/db":
			dbHandler(ctx)
		case "/queries":
			queriesHandler(ctx)
		case "/fortune":
			fortunesHandler(ctx)
		case "/fortune-quick":
			fortunesQuickHandler(ctx)
		case "/update":
			updateHandler(ctx)
		default:
			ctx.Error(fasthttp.StatusMessage(fasthttp.StatusNotFound), fasthttp.StatusNotFound)
		}
	}

	server := &fasthttp.Server{
		Handler: handler,
		Name:    "go",
	}

	if prefork {
		preforkServer := fastprefork.New(server)

		if err := preforkServer.ListenAndServe(bindHost); err != nil {
			panic(err)
		}

	} else {
		if err := server.ListenAndServe(bindHost); err != nil {
			panic(err)
		}
	}
}
