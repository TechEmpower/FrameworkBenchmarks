package main

import (
	"flag"
	"log"
	"net"
	"runtime"

	"github.com/savsgio/atreugo/v7"

	"atreugo/src/handlers"
	"atreugo/src/storage"
	"atreugo/src/templates"
)

func initPools() {
	handlers.InitMessagePool()
	templates.InitFortunesPool()
	storage.InitWorldPool()
	storage.InitWorldsPool()
}

func main() {
	initPools()

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
	db, err := storage.InitDB(*dbDriver, *dbConnectionString, runtime.NumCPU()*4)
	if err != nil {
		log.Fatal(err)
	}
	if *child {
		db, err = storage.InitDB(*dbDriver, *dbConnectionString, runtime.NumCPU())
		if err != nil {
			log.Fatal(err)
		}
	}

	// init json encoders
	var jsonHandler func(ctx *atreugo.RequestCtx) error
	var dbHandler func(ctx *atreugo.RequestCtx) error
	var queriesHandler func(ctx *atreugo.RequestCtx) error
	var updateHandler func(ctx *atreugo.RequestCtx) error
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

	// init atreugo server
	server := atreugo.New(&atreugo.Config{
		Compress: false,
	})

	// init handlers
	server.Path("GET", "/plaintext", handlers.PlaintextHandler)
	server.Path("GET", "/json", jsonHandler)
	if db != nil {
		defer db.Close()
		server.Path("GET", "/fortune", handlers.FortuneHandlerPool(db))
		server.Path("GET", "/fortune-quick", handlers.FortuneQuickHandlerPool(db))
		server.Path("GET", "/db", dbHandler)
		server.Path("GET", "/queries", queriesHandler)
		server.Path("GET", "/update", updateHandler)
	}

	// check for prefork
	var listener net.Listener
	if *prefork {
		listener, err = doPrefork(*child, *bindHost)
		if err != nil {
			log.Fatal(err)
		}
	} else {
		listener, err = net.Listen("tcp4", *bindHost)
		if err != nil {
			log.Fatal(err)
		}
		runtime.GOMAXPROCS(runtime.NumCPU())
	}

	log.Fatal(server.Serve(listener))
}
