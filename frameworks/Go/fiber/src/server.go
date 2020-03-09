package main

import (
	"context"
	"fmt"
	"math/rand"
	"os"
	"runtime"
	"sort"
	"strconv"
	"sync"

	"github.com/gofiber/fiber"
	pgx "github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

func main() {

	initDatabase()

	app := fiber.New()
	app.Settings.ServerHeader = "go"

	app.Get("/plaintext", plaintextHandler)
	app.Get("/json", jsonHandler)
	app.Get("/db", dbHandler)
	app.Get("/queries", queriesHandler)
	app.Get("/update", updateHandler)

	app.Listen(8080)
}

const (
	worldcount     = 10000
	helloworld     = "Hello, World!"
	worldselectsql = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldupdatesql = "UPDATE World SET randomNumber = $1 WHERE id = $2"
)

var (
	db *pgxpool.Pool
)

// Message ...
type Message struct {
	Message string `json:"message"`
}

// Worlds ...
type Worlds []World

// World ...
type World struct {
	Id           int32 `json:"id"`
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
	w.Id = 0
	w.RandomNumber = 0
	WorldPool.Put(w)
}

// WorldsPool ...
var WorldsPool = sync.Pool{
	New: func() interface{} {
		return make(Worlds, 0, 512)
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
	var child bool
	for _, arg := range os.Args[1:] {
		if arg == "-child" {
			child = true
		}
	}
	maxConn := runtime.NumCPU()
	if maxConn == 0 {
		maxConn = 8
	}
	maxConn = maxConn * 4
	if child {
		maxConn = runtime.NumCPU()
	}

	var err error
	db, err = pgxpool.Connect(context.Background(), fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d", "tfb-database", 5432, "benchmarkdbuser", "benchmarkdbpass", "hello_world", maxConn))
	if err != nil {
		panic(err)
	}
}

// jsonHandler :
func jsonHandler(c *fiber.Ctx) {
	m := AcquireJSON()
	m.Message = "Hello, World!"
	c.JSON(m)
	ReleaseJSON(m)
}

// dbHandler :
func dbHandler(c *fiber.Ctx) {
	w := AcquireWorld()
	db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	c.JSON(w)
	ReleaseWorld(w)
}

// queriesHandler :
func queriesHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	}
	c.JSON(Worlds(worlds))
	ReleaseWorlds(worlds)
}

// updateHandler :
func updateHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
		w.RandomNumber = int32(RandomWorld())
	}
	// sorting is required for insert deadlock prevention.
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].Id < worlds[j].Id
	})

	batch := pgx.Batch{}
	for _, w := range worlds {
		batch.Queue(worldupdatesql, w.RandomNumber, w.Id)
	}
	db.SendBatch(context.Background(), &batch).Close()
	c.JSON(Worlds(worlds))
	ReleaseWorlds(worlds)
}

// plaintextHandler :
func plaintextHandler(c *fiber.Ctx) {
	c.SendString(helloworld)
}

// RandomWorld :
func RandomWorld() int {
	return rand.Intn(worldcount) + 1
}

// QueriesCount :
func QueriesCount(c *fiber.Ctx) int {
	n, _ := strconv.Atoi(c.Query("queries"))
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}
