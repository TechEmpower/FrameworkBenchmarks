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

	"gearbox/src/templates"

	"github.com/gogearbox/gearbox"
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
	contentType      = "Content-Type"
	contentTypeHTML  = "text/html; charset=utf-8"
)

func hasArgument(arg string) bool {
	for i := range os.Args[1:] {
		if os.Args[1:][i] == arg {
			return true
		}
	}
	return false
}

func main() {
	initDatabase()

	settings := &gearbox.Settings{
		ServerName:               "go",
		DisableHeaderNormalizing: true,
	}

	if hasArgument("-prefork") {
		settings.Prefork = true
	}
	if hasArgument("-prefork-child") {
		child = true
	}

	gb := gearbox.New(settings)
	gb.Get("/plaintext", plaintextHandler)
	gb.Get("/json", jsonHandler)
	gb.Get("/db", dbHandler)
	gb.Get("/update", updateHandler)
	gb.Get("/queries", queriesHandler)
	gb.Get("/fortunes", templateHandler)
	gb.Get("/cached-worlds", cachedHandler)
	gb.Start(":8080")
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
func jsonHandler(ctx gearbox.Context) {
	m := AcquireJSON()
	m.Message = helloworld
	ctx.SendJSON(&m)
	ReleaseJSON(m)
}

// dbHandler :
func dbHandler(ctx gearbox.Context) {
	w := AcquireWorld()
	db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	ctx.SendJSON(&w)
	ReleaseWorld(w)
}

// Frameworks/Go/fasthttp/src/server-postgresql/server.go#104
func templateHandler(ctx gearbox.Context) {
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

	ctx.Set(contentType, contentTypeHTML)

	templates.WriteFortunePage(ctx.Context(), fortunes)
}

// queriesHandler :
func queriesHandler(ctx gearbox.Context) {
	n := QueriesCount(ctx)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldselectsql, RandomWorld()).Scan(&w.ID, &w.RandomNumber)
	}
	ctx.SendJSON(&worlds)
	ReleaseWorlds(worlds)
}

// updateHandler :
func updateHandler(ctx gearbox.Context) {
	n := QueriesCount(ctx)
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
	ctx.SendJSON(&worlds)
	ReleaseWorlds(worlds)
}

var helloworldRaw = []byte("Hello, World!")

// plaintextHandler :
func plaintextHandler(ctx gearbox.Context) {
	ctx.SendBytes(helloworldRaw)
}

// cachedHandler :
func cachedHandler(ctx gearbox.Context) {
	n := QueriesCount(ctx)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		worlds[i] = cachedWorlds[RandomWorld()-1]
	}
	ctx.SendJSON(&worlds)
	ReleaseWorlds(worlds)
}

// RandomWorld :
func RandomWorld() int {
	return rand.Intn(worldcount) + 1
}

// QueriesCount :
func QueriesCount(ctx gearbox.Context) int {
	n, _ := strconv.Atoi(ctx.Query(queryparam))
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}
