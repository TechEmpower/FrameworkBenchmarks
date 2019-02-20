package main

import (
	"flag"
	"log"
	"net"
	"net/http"
	"runtime"

	"go-std/src/storage"
)

func main() {
	// init flags
	bindHost := flag.String("bind", ":8080", "set bind host")
	prefork := flag.Bool("prefork", false, "use prefork")
	child := flag.Bool("child", false, "is child proc")
	dbDriver := flag.String("db", "none", "db connection driver [values: pq || pgx || none]")
	dbConnectionString := flag.String("db_connection_string",
		"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
		"Set bind host")
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

	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/plaintext", plaintextHandler)
	if db != nil {
		defer db.Close()
		http.HandleFunc("/db", dbHandler(db))
		http.HandleFunc("/queries", queriesHandler(db))
		http.HandleFunc("/fortune", fortuneHandler(db))
		http.HandleFunc("/fortune-quick", fortuneQuickHandler(db))
		http.HandleFunc("/update", updateHandler(db))
	}

	if *prefork {
		http.Serve(listener, nil)
	} else {
		http.ListenAndServe(*bindHost, nil)
	}
}
