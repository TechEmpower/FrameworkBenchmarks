package main

import (
	"context"
	"log"
	"runtime"
	"time"

	"github.com/kataras/iris"

	"database/sql"

	_ "github.com/lib/pq"

	"github.com/kataras/iris/middleware/recover"
)

func main() {
	app := iris.New()
	app.Use(recover.New())

	app.RegisterView(iris.HTML("./templates", ".html"))

	var err error
	var db *sql.DB
	if db, err = initDatabase("tfb-database",
		"benchmarkdbuser",
		"benchmarkdbpass",
		"hello_world",
		runtime.NumCPU()); err != nil {
		log.Fatalf("Error opening database: %s", err)
	}
	defer db.Close()

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

	app.Run(iris.Addr(":8080"), iris.WithoutServerError(iris.ErrServerClosed), iris.WithoutInterruptHandler)
}
