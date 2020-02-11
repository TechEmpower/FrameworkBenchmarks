package main

import (
	"context"
	"sort"

	"github.com/gofiber/fiber"
	pgx "github.com/jackc/pgx/v4"
	"github.com/tidwall/sjson"
)

func main() {
	initDatabase()

	app := fiber.New()

	app.Server = "Go"
	app.Prefork = true

	app.Get("/json", jsonHandler)
	app.Get("/db", dbHandler)
	app.Get("/queries", queriesHandler)
	app.Get("/update", updateHandler)
	app.Get("/plaintext", plaintextHandler)

	app.Listen(8080)
}

func jsonHandler(c *fiber.Ctx) {
	json, _ := sjson.SetBytesOptions(jsonRaw, jsonPath, helloRaw, jsonOpt)
	c.JsonBytes(json)
}

func dbHandler(c *fiber.Ctx) {
	w := AcquireWorld()
	db.QueryRow(context.Background(), worldSelectSQL, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	c.Json(w)
	ReleaseWorld(w)
}

func queriesHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldSelectSQL, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	}
	c.Json(Worlds(worlds))
	ReleaseWorlds(worlds)
}

func updateHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldSelectSQL, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
		w.RandomNumber = int32(RandomWorld())
	}
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].Id < worlds[j].Id
	})

	batch := pgx.Batch{}
	for _, w := range worlds {
		batch.Queue(worldUpdateSQL, w.RandomNumber, w.Id)
	}
	db.SendBatch(context.Background(), &batch).Close()
	c.Json(Worlds(worlds))
	ReleaseWorlds(worlds)
}

func plaintextHandler(c *fiber.Ctx) {
	c.SendString(helloText)
}
