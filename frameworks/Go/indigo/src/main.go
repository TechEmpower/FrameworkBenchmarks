package main

import (
	"context"
	"fmt"
	"log"

	"github.com/indigo-web/indigo"
	"github.com/indigo-web/indigo/router/inbuilt"
	"github.com/jackc/pgx/v5/pgxpool"
)

func main() {
	pool, err := pgxpool.New(context.Background(),
		fmt.Sprintf(
			"host=%s port=%d user=%s password=%s dbname=%s",
			"tfb-database", 5432,
			"benchmarkdbuser",
			"benchmarkdbpass",
			"hello_world",
		))
	if err != nil {
		log.Fatal(err)
	}

	DB := NewDB(pool)
	app := NewApp(DB)

	err = app.PopulateCache(context.Background())
	if err != nil {
		log.Fatal(err)
	}

	router := inbuilt.New()

	router.Resource("/json").Get(app.HandleJSON)
	router.Resource("/db").Get(app.HandleDB)
	router.Resource("/query").Get(app.HandleQuery)
	router.Resource("/update").Get(app.HandleUpdate)
	router.Resource("/cached-query").Get(app.HandleCachedQuery)
	router.Resource("/fortunes").Get(app.HandleFortune)
	router.Resource("/plaintext").Get(app.HandlePlaintext)

	err = indigo.New(":8080").Serve(router)
	if err != nil {
		log.Fatal(err)
	}
}
