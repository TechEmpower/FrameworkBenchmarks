package main

import (
	"flag"
	"fmt"
	"log"
	"runtime"

	"atreugo/src/handlers"
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v11"
)

var bindHost, jsonEncoder, dbDriver, dbConnectionString string
var prefork, useQuickTemplate bool

func init() {
	// init flags
	flag.StringVar(&bindHost, "bind", ":8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")
	flag.StringVar(&jsonEncoder, "json_encoder", "none", "json encoder: none, easyjson or sjson")
	flag.StringVar(&dbDriver, "db", "none", "db connection driver [values: none or pgx or mongo]")
	flag.StringVar(&dbConnectionString, "db_connection_string", "", "db connection string")
	flag.BoolVar(&useQuickTemplate, "quicktemplate", false, "use quicktemplate")

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
	var jsonHandler atreugo.View
	var dbHandler atreugo.View
	var queriesHandler atreugo.View
	var updateHandler atreugo.View
	var fortuneHandler atreugo.View

	if useQuickTemplate {
		fortuneHandler = handlers.FortuneQuickHandler(db)
	} else {
		fortuneHandler = handlers.FortuneHandler(db)
	}

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

	// init atreugo server
	server := atreugo.New(atreugo.Config{
		Addr:    bindHost,
		Name:    "Go",
		Prefork: prefork,
	})

	// init handlers
	server.GET("/plaintext", handlers.PlaintextHandler)
	server.GET("/json", jsonHandler)
	server.GET("/db", dbHandler)
	server.GET("/queries", queriesHandler)
	server.GET("/fortune", fortuneHandler)
	server.GET("/update", updateHandler)

	if err := server.ListenAndServe(); err != nil {
		panic(err)
	}
}
