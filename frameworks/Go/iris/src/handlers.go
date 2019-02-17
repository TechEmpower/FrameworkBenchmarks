package main

import (
	"log"
	"sort"
	"strconv"

	"iris/src/templates"

	"iris/src/storage"

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

func dbHandler(db storage.DB) func(ctx context.Context) {
	return func(ctx context.Context) {
		world, err := db.GetOneRandomWorld()
		if err != nil {
			log.Fatal(err)
		}

		ctx.Header("Server", "Iris")
		ctx.JSON(world)
	}
}

func queriesHandler(db storage.DB) func(ctx context.Context) {
	return func(ctx context.Context) {
		q, err := strconv.Atoi(ctx.URLParam("queries"))
		if err != nil || q < 1 {
			q = 1
		} else if q > 500 {
			q = 500
		}

		results := make([]storage.World, q)

		for i := 0; i < q; i++ {
			results[i], err = db.GetOneRandomWorld()
			if err != nil {
				log.Println(err)
			}
		}

		ctx.Header("Server", "Iris")
		ctx.JSON(results)
	}
}

func updateHandler(db storage.DB) func(ctx context.Context) {
	return func(ctx context.Context) {
		q, err := strconv.Atoi(ctx.URLParam("queries"))
		if err != nil || q < 1 {
			q = 1
		} else if q > 500 {
			q = 500
		}

		worlds, err := db.UpdateRandomWorlds(q)
		if err != nil {
			log.Fatal(err)
		}

		ctx.Header("Server", "Iris")
		ctx.JSON(worlds)
	}
}

func fortuneHandler(db storage.DB) func(ctx context.Context) {
	return func(ctx context.Context) {
		fortunes, err := db.GetFortunes()
		if err != nil {
			log.Fatal(err)
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.Header("Server", "Iris")
		ctx.View("fortunes.html", struct {
			Fortunes []templates.Fortune
		}{fortunes})
	}
}

func fortuneQuickHandler(db storage.DB) func(ctx context.Context) {
	return func(ctx context.Context) {
		fortunes, err := db.GetFortunes()
		if err != nil {
			log.Fatal(err)
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.Header("Server", "Iris")
		ctx.ContentType("text/html; charset=utf-8")
		templates.WriteFortunePage(ctx.ResponseWriter(), fortunes)
	}
}
