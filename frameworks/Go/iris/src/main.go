package main

import (
	"context"
	"flag"
	"log"
	"net"
	"runtime"
	"time"

	"github.com/kataras/iris"

	_ "github.com/lib/pq"

	"github.com/kataras/iris/middleware/recover"
)

func main() {
	// init flags
	bindHost := flag.String("bind", ":8080", "set bind host")
	prefork := flag.Bool("prefork", false, "use prefork")
	child := flag.Bool("child", false, "is child proc")
	dbConnectionString := flag.String("db_connection_string",
		"host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world sslmode=disable",
		"Set bind host")
	flag.Parse()

	// check for prefork
	var listener net.Listener
	if *prefork {
		listener = doPrefork(*child, *bindHost)
	}

	// init iris app
	app := iris.New()
	app.Use(recover.New())
	app.RegisterView(iris.HTML("./templates", ".html"))

	// init database
	db, err := initDatabase(
		*dbConnectionString,
		runtime.NumCPU())
	if err != nil {
		log.Fatalf("Error opening database: %s", err)
	}
	defer db.Close()

	// add handlers
	app.Handle("GET", "/json", jsonHandler)
	app.Handle("GET", "/plaintext", plaintextHandler)
	app.Handle("GET", "/db", dbHandler)
	app.Handle("GET", "/queries", queriesHandler)
	app.Handle("GET", "/update", updateHandler(db))
	app.Handle("GET", "/fortune", fortuneHandler)

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
