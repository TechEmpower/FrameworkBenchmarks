package main

import (
	"context"
	"fmt"
	"math/rand"
	"runtime"
	"runtime/debug"
	"sort"
	"strconv"
	"sync"

	"fiber/src/templates"

	"github.com/gofiber/fiber"
	"github.com/gofiber/utils"
	pgx "github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

var (
	child        bool
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
)

func main() {
	initDatabase()

	app := fiber.New(&fiber.Settings{
		CaseSensitive:            true,
		StrictRouting:            true,
		DisableHeaderNormalizing: true,
		ServerHeader:             "go",
	})
	if utils.GetArgument("-prefork") {
		app.Settings.Prefork = true
	}
	if utils.GetArgument("-prefork-child") {
		child = true
	}
	if utils.GetArgument("-nogc") {
		debug.SetGCPercent(-1)
	}

	if utils.GetArgument("-prefork-child") {
		child = true
	}
	if utils.GetArgument("-nogc") {
		debug.SetGCPercent(-1)
	}

	app.Get("/plaintext", plaintextHandler)
	app.Get("/json", jsonHandler)
	app.Get("/db", dbHandler)
	app.Get("/update", updateHandler)
	app.Get("/queries", queriesHandler)
	app.Get("/fortunes", templateHandler)
	app.Get("/cached-worlds", cachedHandler)
	app.Listen(8080)
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
	maxConn := runtime.NumCPU()
	if maxConn == 0 {
		maxConn = 8
	}
	if child {
		maxConn = maxConn
	} else {
		maxConn = maxConn * 4
	}

	var err error
	db, err = pgxpool.Connect(context.Background(), fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d", "tfb-database", 5432, "benchmarkdbuser", "benchmarkdbpass", "hello_world", maxConn))
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
func jsonHandler(c *fiber.Ctx) {
	m := AcquireJSON()
	m.Message = helloworld
	c.JSON(&m)
	ReleaseJSON(m)
}

// dbHandler :
func dbHandler(c *fiber.Ctx) {
	w := AcquireWorld()
	db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	c.JSON(&w)
	ReleaseWorld(w)
}

// Frameworks/Go/fasthttp/src/server-postgresql/server.go#104
func templateHandler(c *fiber.Ctx) {
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

	c.Set(fiber.HeaderContentType, fiber.MIMETextHTMLCharsetUTF8)

	templates.WriteFortunePage(c.Fasthttp, fortunes)
}

// queriesHandler :
func queriesHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	}
	c.JSON(&worlds)
	ReleaseWorlds(worlds)
}

// updateHandler :
func updateHandler(c *fiber.Ctx) {
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
}

var helloworldRaw = []byte("Hello, World!")

// plaintextHandler :
func plaintextHandler(c *fiber.Ctx) {
	c.SendBytes(helloworldRaw)
}

// cachedHandler :
func cachedHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		worlds[i] = cachedWorlds[RandomWorld()-1]
	}
	c.JSON(&worlds)
	ReleaseWorlds(worlds)
}

// RandomWorld :
func RandomWorld() int {
	return rand.Intn(worldcount) + 1
}

// QueriesCount :
func QueriesCount(c *fiber.Ctx) int {
	n, _ := strconv.Atoi(c.Query(queryparam))
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}
