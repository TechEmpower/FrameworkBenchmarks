package main

import (
	"flag"
	"log"
	"net"
	"net/http"
	"runtime"

	"go-std/src/handlers"
	"go-std/src/storage"
)

func main() {
	// init flags
	bindHost := flag.String("bind", ":8080", "set bind host")
	prefork := flag.Bool("prefork", false, "use prefork")
	easyjson := flag.Bool("easyjson", false, "use easyjson")
	child := flag.Bool("child", false, "is child proc")
	dbDriver := flag.String("db", "none", "db connection driver [values: pq || pgx || none]")
	dbConnectionString := flag.String("db_connection_string",
		"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
		"db connection string")
	flag.Parse()

	// check for prefork
	var listener net.Listener
	if *prefork {
		listener = doPrefork(*child, *bindHost)
	} else {
		runtime.GOMAXPROCS(runtime.NumCPU())
	}

	// init database with appropriate driver
	db, err := storage.InitDB(*dbDriver, *dbConnectionString)
	if err != nil {
		log.Fatal(err)
	}

	// init handlers
	http.HandleFunc("/plaintext", handlers.plaintextHandler)
	if *easyjson {
		http.HandleFunc("/json", handlers.jsonHandlerEasyJSON)
	} else {
		http.HandleFunc("/json", handlers.jsonHandler)
	}
	if db != nil {
		defer db.Close()
		if *easyjson {
			http.HandleFunc("/db", handlers.dbHandlerEasyJSON(db))
			http.HandleFunc("/queries", handlers.queriesHandlerEasyJSON(db))
			http.HandleFunc("/fortune", handlers.fortuneHandlerEasyJSON(db))
			http.HandleFunc("/fortune-quick", handlers.fortuneQuickHandlerEasyJSON(db))
			http.HandleFunc("/update", handlers.updateHandlerEasyJSON(db))
		} else {
			http.HandleFunc("/db", handlers.dbHandler(db))
			http.HandleFunc("/queries", handlers.queriesHandler(db))
			http.HandleFunc("/fortune", handlers.fortuneHandler(db))
			http.HandleFunc("/fortune-quick", handlers.fortuneQuickHandler(db))
			http.HandleFunc("/update", handlers.updateHandler(db))
		}
	}

	if *prefork {
		http.Serve(listener, nil)
	} else {
		http.ListenAndServe(*bindHost, nil)
	}
}
