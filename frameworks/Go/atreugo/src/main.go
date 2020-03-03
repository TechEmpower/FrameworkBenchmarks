package main

import (
	"flag"
	"fmt"
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
	flag.StringVar(&bindHost, "bind", ":8080", "set bind host")
	flag.BoolVar(&prefork, "prefork", false, "use prefork")
	flag.BoolVar(&child, "child", false, "is child proc")
	flag.StringVar(&jsonEncoder, "json_encoder", "none", "json encoder: none or easyjson or gojay or sjson")
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
	if child {
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
		Name: "Go",
	})

	// init handlers
	server.GET("/plaintext", handlers.PlaintextHandler)
	server.GET("/json", jsonHandler)
	if db != nil {
		defer db.Close()

		server.GET("/fortune", handlers.FortuneHandler(db))
		server.GET("/fortune-quick", handlers.FortuneQuickHandler(db))
		server.GET("/db", dbHandler)
		server.GET("/queries", queriesHandler)
		server.GET("/update", updateHandler)
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
