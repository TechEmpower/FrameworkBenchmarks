package main

import (
	"flag"
	"log"
	"net"
	"runtime"

	"atreugo/src/handlers"
	"atreugo/src/storage"
	"atreugo/src/templates"

	"github.com/savsgio/atreugo"
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
	dbDriver := flag.String("db", "none", "db connection driver [values: pgx || none]")
	dbConnectionString := flag.String("db_connection_string",
		"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
		"db connection string")
	flag.Parse()

	// init database with appropriate driver
	db, err := storage.InitDB(*dbDriver, *dbConnectionString)
	if err != nil {
		log.Fatal(err)
	}

	// init atreugo server
	server := atreugo.New(&atreugo.Config{
		Compress: false,
	})

	// init handlers
	server.Path("GET", "/plaintext", handlers.PlaintextHandler)
	server.Path("GET", "/json", handlers.JSONHandlerEasyJSON)
	if db != nil {
		defer db.Close()
		server.Path("GET", "/fortune", handlers.FortuneHandlerPool(db))
		server.Path("GET", "/fortune-quick", handlers.FortuneQuickHandlerPool(db))
		server.Path("GET", "/db", handlers.DBHandlerEasyJSON(db))
		server.Path("GET", "/queries", handlers.QueriesHandlerEasyJSON(db))
		server.Path("GET", "/update", handlers.UpdateHandlerEasyJSON(db))
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
