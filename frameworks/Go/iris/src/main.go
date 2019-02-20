package main

import (
	"context"
	"flag"
	"log"
	"net"
	"runtime"
	"time"

	"iris/src/storage"

	"github.com/kataras/iris"
	"github.com/kataras/iris/middleware/recover"
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

	// init iris app
	app := iris.New()
	app.Use(recover.New())

	// init database with appropriate driver
	db, err := storage.InitDB(*dbDriver, *dbConnectionString)
	if err != nil {
		log.Fatal(err)
	}

	// add handlers
	app.Handle("GET", "/json", jsonHandler)
	app.Handle("GET", "/plaintext", plaintextHandler)
	if db != nil {
		defer db.Close()
		app.Handle("GET", "/db", dbHandler(db))
		app.Handle("GET", "/queries", queriesHandler(db))
		app.Handle("GET", "/update", updateHandler(db))

		app.RegisterView(iris.HTML("./templates", ".html"))
		app.Handle("GET", "/fortune", fortuneHandler(db))
		app.Handle("GET", "/fortune-quick", fortuneQuickHandler(db))
	}

	iris.RegisterOnInterrupt(func() {
		timeout := 10 * time.Second
		ctx, cancel := context.WithTimeout(context.Background(), timeout)
		defer cancel()
		app.Shutdown(ctx)
	})

	// run iris app
	if *prefork {
		app.Run(iris.Listener(listener), iris.WithoutServerError(iris.ErrServerClosed), iris.WithoutInterruptHandler)
	} else {
		app.Run(iris.Addr(*bindHost), iris.WithoutServerError(iris.ErrServerClosed), iris.WithoutInterruptHandler)
	}
}
