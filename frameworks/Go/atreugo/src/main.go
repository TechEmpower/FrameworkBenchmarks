package main

import (
	"flag"
	"log"
	"runtime"
	"strconv"
	"strings"

	"atreugo/src/handlers"
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v8"
)

func main() {
	// init flags
	bindHost := flag.String("bind", ":8080", "set bind host")
	prefork := flag.Bool("prefork", false, "use prefork")
	child := flag.Bool("child", false, "is child proc")
	jsonEncoder := flag.String("json_encoder", "none", "json encoder: none or easyjson or gojay or sjson")
	dbDriver := flag.String("db", "none", "db connection driver [values: none or pgx or mongo]")
	dbConnectionString := flag.String("db_connection_string",
		"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
		"db connection string")
	flag.Parse()

	// init database with appropriate driver
	dbMaxConnectionCount := runtime.NumCPU() * 4
	if *child {
		dbMaxConnectionCount = runtime.NumCPU()
	}

	db, err := storage.InitDB(*dbDriver, *dbConnectionString, dbMaxConnectionCount)
	if err != nil {
		log.Fatal(err)
	}

	// init json encoders
	var jsonHandler atreugo.View
	var dbHandler atreugo.View
	var queriesHandler atreugo.View
	var updateHandler atreugo.View

	switch *jsonEncoder {
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

	addr := strings.Split(*bindHost, ":")
	host := addr[0]
	port, _ := strconv.Atoi(addr[1])

	// init atreugo server
	server := atreugo.New(&atreugo.Config{
		Host:     host,
		Port:     port,
		Compress: false,
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

	// check for prefork
	if *prefork {
		ln, err := doPrefork(*child, *bindHost)
		if err != nil {
			log.Fatal(err)
		}
		log.Fatal(server.Serve(ln))
	} else {
		runtime.GOMAXPROCS(runtime.NumCPU())
		log.Fatal(server.ListenAndServe())
	}
}
