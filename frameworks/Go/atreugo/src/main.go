package main

import (
	"flag"
	"log"
	"net"
	"os"
	"runtime"

	"atreugo/src/handlers"
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v10"
)

var bindHost, jsonEncoder, dbDriver, dbConnectionString string
var prefork, child bool

func init() {
	// init flags
	flag.StringVar(&bindHost, "bind", "0.0.0.0:8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")
	flag.BoolVar(&child, "child", false, "is child proc")
	flag.StringVar(&jsonEncoder, "json_encoder", "none", "json encoder: none or easyjson or gojay or sjson")
	flag.StringVar(&dbDriver, "db", "none", "db connection driver [values: none or pgx or mongo]")
	flag.StringVar(&dbConnectionString, "db_connection_string",
		"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
		"db connection string")

	flag.Parse()
}

func main() {
	// init database with appropriate driver
	dbMaxConnectionCount := runtime.NumCPU() * 4
	if child {
		dbMaxConnectionCount = runtime.NumCPU()
	}

	db, err := storage.InitDB(dbDriver, dbConnectionString, dbMaxConnectionCount)
	if err != nil {
		log.Fatal(err)
	}

	// init json encoders
	var jsonHandler atreugo.View
	var dbHandler atreugo.View
	var queriesHandler atreugo.View
	var updateHandler atreugo.View

	switch jsonEncoder {
	case "easyjson":
		jsonHandler = handlers.JSONHandlerEasyJSON
		dbHandler = handlers.DBHandlerEasyJSON(db)
		queriesHandler = handlers.QueriesHandlerEasyJSON(db)
		updateHandler = handlers.UpdateHandlerEasyJSON(db)
	case "gojay":
		jsonHandler = handlers.JSONHandlerGoJay
		dbHandler = handlers.DBHandlerGoJay(db)
		queriesHandler = handlers.QueriesHandlerGoJay(db)
		updateHandler = handlers.UpdateHandlerGoJay(db)
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

	// init atreugo server
	server := atreugo.New(&atreugo.Config{
		Addr: bindHost,
	})

	// init handlers
	server.Path("GET", "/plaintext", handlers.PlaintextHandler)
	server.Path("GET", "/json", jsonHandler)
	if db != nil {
		defer db.Close()
		server.Path("GET", "/fortune", handlers.FortuneHandler(db))
		server.Path("GET", "/fortune-quick", handlers.FortuneQuickHandler(db))
		server.Path("GET", "/db", dbHandler)
		server.Path("GET", "/queries", queriesHandler)
		server.Path("GET", "/update", updateHandler)
	}

	if child {
		runtime.GOMAXPROCS(1)

		ln, err := net.FileListener(os.NewFile(3, ""))
		if err != nil {
			panic(err)
		}
		if err := server.Serve(ln); err != nil {
			panic(err)
		}

	} else if prefork {
		if err := doPrefork(bindHost); err != nil {
			panic(err)
		}

	} else {
		if err := server.ListenAndServe(); err != nil {
			panic(err)
		}
	}
}
