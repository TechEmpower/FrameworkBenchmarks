package main

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"os"
	"runtime"
	"sort"
	"sync"

	"github.com/goccy/go-json"
	"github.com/gofiber/fiber/v2"

	"fiber/app/templates"

	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

var (
	db           *pgxpool.Pool
	cachedWorlds Worlds
)

const (
	queryparam       = "q"
	helloworld       = "Hello, World!"
	worldcount       = 10000
	worldselectsql   = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldupdatesql   = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	worldcachesql    = "SELECT * FROM World LIMIT $1"
	fortuneselectsql = "SELECT id, message FROM Fortune"
	pathJSON         = "/json"
	pathDB           = "/db"
	pathQueries      = "/queries"
	pathCache        = "/cached-worlds"
	pathFortunes     = "/fortunes"
	pathUpdate       = "/update"
	pathText         = "/plaintext"
)

func main() {
	initDatabase()

	config := fiber.Config{
		CaseSensitive:            true,
		StrictRouting:            true,
		DisableHeaderNormalizing: true,
		ServerHeader:             "go",
		JSONEncoder:              json.Marshal,
		JSONDecoder:              json.Unmarshal,
	}

	for i := range os.Args[1:] {
		if os.Args[1:][i] == "-prefork" {
			config.Prefork = true
		}
	}

	app := fiber.New(config)

	app.Use(func(c *fiber.Ctx) error {
		switch c.Path() {
		case pathJSON:
			jsonHandler(c)
		case pathDB:
			dbHandler(c)
		case pathQueries:
			queriesHandler(c)
		case pathCache:
			cachedHandler(c)
		case pathFortunes:
			templateHandler(c)
		case pathUpdate:
			updateHandler(c)
		case pathText:
			plaintextHandler(c)
		}
		return nil
	})

	log.Fatal(app.Listen(":8080"))
}

// Message ...
type Message struct {
	Message string `json:"message"`
}

// Worlds ...
type Worlds []World

// World ...
type World struct {
	ID           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

// JSONpool ...
var JSONpool = sync.Pool{
	New: func() interface{} {
		return new(Message)
	},
}

// AcquireJSON ...
func AcquireJSON() *Message {
	return JSONpool.Get().(*Message)
}

// ReleaseJSON ...
func ReleaseJSON(json *Message) {
	json.Message = ""
	JSONpool.Put(json)
}

// WorldPool ...
var WorldPool = sync.Pool{
	New: func() interface{} {
		return new(World)
	},
}

// AcquireWorld ...
func AcquireWorld() *World {
	return WorldPool.Get().(*World)
}

// ReleaseWorld ...
func ReleaseWorld(w *World) {
	w.ID = 0
	w.RandomNumber = 0
	WorldPool.Put(w)
}

// WorldsPool ...
var WorldsPool = sync.Pool{
	New: func() interface{} {
		return make(Worlds, 0, 500)
	},
}

// AcquireWorlds ...
func AcquireWorlds() Worlds {
	return WorldsPool.Get().(Worlds)
}

// ReleaseWorlds ...ReleaseWorlds
func ReleaseWorlds(w Worlds) {
	w = w[:0]
	WorldsPool.Put(w)
}

// initDatabase :
func initDatabase() {
	maxConn := runtime.NumCPU() * 4
	if fiber.IsChild() {
		maxConn = 5
	}

	var err error
	db, err = pgxpool.New(context.Background(),
		fmt.Sprintf(
			"host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d",
			"tfb-database", 5432,
			"benchmarkdbuser",
			"benchmarkdbpass",
			"hello_world",
			maxConn,
		))
	if err != nil {
		panic(err)
	}
	populateCache()
}

// this will populate the cached worlds for the cache test
func populateCache() {
	worlds := make(Worlds, worldcount)
	rows, err := db.Query(context.Background(), worldcachesql, worldcount)
	if err != nil {
		panic(err)
	}
	for i := 0; i < worldcount; i++ {
		w := &worlds[i]
		if !rows.Next() {
			break
		}
		if err := rows.Scan(&w.ID, &w.RandomNumber); err != nil {
			panic(err)
		}
		//db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	}
	cachedWorlds = worlds
}

// jsonHandler :
func jsonHandler(c *fiber.Ctx) error {
	m := AcquireJSON()
	m.Message = helloworld
	c.JSON(&m)
	ReleaseJSON(m)
	return nil
}

// dbHandler :
func dbHandler(c *fiber.Ctx) error {
	w := AcquireWorld()
	db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	c.JSON(&w)
	ReleaseWorld(w)
	return nil
}

// Frameworks/Go/fasthttp/src/server-postgresql/server.go#104
func templateHandler(c *fiber.Ctx) error {
	rows, _ := db.Query(context.Background(), fortuneselectsql)

	var f templates.Fortune
	fortunes := make([]templates.Fortune, 0, 16)
	for rows.Next() {
		_ = rows.Scan(&f.ID, &f.Message)
		fortunes = append(fortunes, f)
	}
	rows.Close()
	fortunes = append(fortunes, templates.Fortune{
		Message: "Additional fortune added at request time.",
	})

	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})

	c.Response().Header.SetContentType(fiber.MIMETextHTMLCharsetUTF8)

	templates.WriteFortunePage(c.Context(), fortunes)
	return nil
}

// queriesHandler :
func queriesHandler(c *fiber.Ctx) error {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	}
	c.JSON(&worlds)
	ReleaseWorlds(worlds)
	return nil
}

// updateHandler :
func updateHandler(c *fiber.Ctx) error {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
		w.RandomNumber = int32(RandomWorld())
	}
	// sorting is required for insert deadlock prevention.
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].ID < worlds[j].ID
	})

	batch := pgx.Batch{}
	for _, w := range worlds {
		batch.Queue(worldupdatesql, w.RandomNumber, w.ID)
	}
	db.SendBatch(context.Background(), &batch).Close()
	c.JSON(&worlds)
	ReleaseWorlds(worlds)

	return nil
}

var helloworldRaw = []byte("Hello, World!")

// plaintextHandler :
func plaintextHandler(c *fiber.Ctx) error {
	return c.Send(helloworldRaw)
}

// cachedHandler :
func cachedHandler(c *fiber.Ctx) error {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		worlds[i] = cachedWorlds[RandomWorld()-1]
	}
	c.JSON(&worlds)
	ReleaseWorlds(worlds)
	return nil
}

// RandomWorld :
func RandomWorld() int {
	return rand.Intn(worldcount) + 1
}

// QueriesCount :
func QueriesCount(c *fiber.Ctx) int {
	n := c.Request().URI().QueryArgs().GetUintOrZero(queryparam)
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}
