package main

import (
	"database/sql"
	"math/rand"
	"runtime"
	"sort"
	"strconv"
	"sync"

	_ "github.com/go-sql-driver/mysql"
	"github.com/gofiber/fiber"
)

func main() {
	initHandlers()

	app := fiber.New()
	app.Server = "Fiber"

	app.Get("/json", jsonHandler)
	app.Get("/db", dbHandler)
	app.Get("/queries", queriesHandler)
	app.Get("/update", updateHandler)
	app.Get("/plaintext", plaintextHandler)

	app.Listen(8080)
}

const (
	worldcount       = 10000
	helloworld       = "Hello, World!"
	connectionstring = "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world"
)

var (
	db                *sql.DB
	stmtWorldSelect   *sql.Stmt
	stmtWorldUpdate   *sql.Stmt
	stmtFortuneSelect *sql.Stmt
)

type Message struct {
	Message string `json:"message"`
}

type Worlds []World

type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

type Fortune struct {
	Id      int32  `json:"id"`
	Message string `json:"message"`
}

var JSONpool = sync.Pool{
	New: func() interface{} {
		return new(Message)
	},
}

func AcquireJSON() *Message {
	return JSONpool.Get().(*Message)
}
func ReleaseJSON(json *Message) {
	json.Message = ""
	JSONpool.Put(json)
}

func initHandlers() {
	db, _ = sql.Open("mysql", connectionstring)
	db.SetMaxIdleConns(runtime.NumCPU() * 2)
	db.SetMaxOpenConns(runtime.NumCPU() * 2)
	stmtWorldSelect, _ = db.Prepare("SELECT id, randomNumber FROM World WHERE id = ?")
	stmtWorldUpdate, _ = db.Prepare("UPDATE World SET randomNumber = ? WHERE id = ?")
	stmtFortuneSelect, _ = db.Prepare("SELECT id, message FROM Fortune")
}

func jsonHandler(c *fiber.Ctx) {
	json := AcquireJSON()
	json.Message = helloworld
	c.Json(json)
	ReleaseJSON(json)
}
func dbHandler(c *fiber.Ctx) {
	var json World
	stmtWorldSelect.QueryRow(RandomWorld()).Scan(&json.Id, &json.RandomNumber)
	c.Json(json)
}
func queriesHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		stmtWorldSelect.QueryRow(RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	}
	c.Json(Worlds(worlds))
}
func updateHandler(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		stmtWorldSelect.QueryRow(RandomWorld()).Scan(&w.Id, &w.RandomNumber)
		w.RandomNumber = int32(RandomWorld())
	}
	// sorting is required for insert deadlock prevention.
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].Id < worlds[j].Id
	})

	txn, _ := db.Begin()
	stmt := txn.Stmt(stmtWorldUpdate)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		stmt.Exec(w.RandomNumber, w.Id)
	}
	txn.Commit()
	c.Json(Worlds(worlds))
}
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
