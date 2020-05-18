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

const (
	WorldCount = 10000
	Text       = "Hello, World!"
)

var (
	db                *sql.DB
	stmtWorldSelect   *sql.Stmt
	stmtWorldUpdate   *sql.Stmt
	stmtFortuneSelect *sql.Stmt
)

// JSON struct for json serialization
type JSON struct {
	Message string `json:"message"`
}

// World struct for single database query
type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

// Worlds struct for multiple database queries
type Worlds []World

func main() {
	connectDB()
	app := fiber.New()
	app.Server = "Fiber"
	app.Get("/json", jsonSerialization)
	app.Get("/db", singleDatabaseQuery)
	app.Get("/queries", multipleDatabaseQueries)
	app.Get("/update", databaseUpdates)
	app.Get("/plaintext", plainText)
	app.Listen(8080)
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#json-serialization
func jsonSerialization(c *fiber.Ctx) {
	j := AcquireJSON()
	j.Message = Text
	c.Json(j)
	ReleaseJSON(j)
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#single-database-query
func singleDatabaseQuery(c *fiber.Ctx) {
	var json World
	stmtWorldSelect.QueryRow(RandomWorld()).Scan(&json.Id, &json.RandomNumber)
	c.Json(json)
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#multiple-database-queries
func multipleDatabaseQueries(c *fiber.Ctx) {
	n := QueriesCount(c)
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		stmtWorldSelect.QueryRow(RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	}
	c.Json(Worlds(worlds))
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
func databaseUpdates(c *fiber.Ctx) {
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

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#plaintext
func plainText(c *fiber.Ctx) {
	c.SendString(Text)
}

// ************************
//    Helper functions
// ************************

func connectDB() {
	db, _ = sql.Open("mysql", "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world")
	db.SetMaxIdleConns(runtime.NumCPU() * 2)
	db.SetMaxOpenConns(runtime.NumCPU() * 2)
	stmtWorldSelect, _ = db.Prepare("SELECT id, randomNumber FROM World WHERE id = ?")
	stmtWorldUpdate, _ = db.Prepare("UPDATE World SET randomNumber = ? WHERE id = ?")
	stmtFortuneSelect, _ = db.Prepare("SELECT id, message FROM Fortune")
}

// JSONpool :
var JSONpool = sync.Pool{
	New: func() interface{} {
		return new(JSON)
	},
}

// AcquireJSON returns new message from pool
func AcquireJSON() *JSON {
	return JSONpool.Get().(*JSON)
}

// ReleaseJSON resets the message and return it to the pool
func ReleaseJSON(j *JSON) {
	j.Message = ""
	JSONpool.Put(j)
}

// RandomWorld :
func RandomWorld() int {
	return rand.Intn(WorldCount) + 1
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
