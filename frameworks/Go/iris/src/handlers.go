package main

import (
	"database/sql"
	"log"
	"sort"
	"strconv"

	"github.com/kataras/iris/context"
)

func jsonHandler(ctx context.Context) {
	ctx.Header("Server", "Iris")
	ctx.JSON(context.Map{"message": "Hello, World!"})
}

func plaintextHandler(ctx context.Context) {
	ctx.Header("Server", "Iris")
	ctx.Text("Hello, World!")
}

func dbHandler(ctx context.Context) {
	ctx.Header("Server", "Iris")
	ctx.JSON(getOneRandomWorld())
}

func queriesHandler(ctx context.Context) {
	q, err := strconv.Atoi(ctx.URLParam("queries"))
	if err != nil || q < 1 {
		q = 1
	} else if q > 500 {
		q = 500
	}

	results := make([]World, q)
	for i := 0; i < q; i++ {
		results[i] = getOneRandomWorld()
	}

	ctx.Header("Server", "Iris")
	ctx.JSON(results)
}

func updateHandler(db *sql.DB) func(ctx context.Context) {
	return func(ctx context.Context) {
		q, err := strconv.Atoi(ctx.URLParam("queries"))
		if err != nil || q < 1 {
			q = 1
		} else if q > 500 {
			q = 500
		}

		ctx.Header("Server", "Iris")
		ctx.JSON(updateRandomWorlds(db, q))
	}
}

func fortuneHandler(ctx context.Context) {
	var err error
	var rows *sql.Rows
	rows, err = fortuneStmt.Query()
	if err != nil {
		log.Fatalf("Can't query fortunes: %s\n", err)
	}

	var fortunes []Fortune

	var fortune Fortune
	for rows.Next() {
		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Fatalf("Can't scan fortune: %s\n", err)
		}
		fortunes = append(fortunes, fortune)
	}
	rows.Close()
	fortunes = append(fortunes, Fortune{Message: "Additional fortune added at request time."})

	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})

	ctx.Header("Server", "Iris")
	ctx.View("fortunes.html", struct {
		Fortunes []Fortune
	}{fortunes})
}
