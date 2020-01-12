package main

import (
	"database/sql"
	"math/rand"
	"runtime"
	"sort"
	"strconv"

	"github.com/fenny/fiber"
	_ "github.com/go-sql-driver/mysql"
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
	// Connect with database
	connect()
	// Create new fiber instance
	app := fiber.New()
	// All test types require Server HTTP response headers.
	app.Server = "Fiber"
	// Set routes with the correct handlers

	app.Get("/json", jsonSerialization)
	app.Get("/db", singleDatabaseQuery)
	app.Get("/queries", multipleDatabaseQueries)
	app.Get("/update", databaseUpdates)
	app.Get("/plaintext", plainText)
	// Listen on port 8080
	app.Listen(8080)
}

// Connect with Mysql database
func connect() {
	// Connect with database
	db, _ = sql.Open("mysql", "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world")

	// Set some connection settings
	maxConn := runtime.NumCPU() * 2
	db.SetMaxIdleConns(maxConn)
	db.SetMaxOpenConns(maxConn)

	// Prepare statement
	stmtWorldSelect, _ = db.Prepare("SELECT id, randomNumber FROM World WHERE id = ?")
	stmtWorldUpdate, _ = db.Prepare("UPDATE World SET randomNumber = ? WHERE id = ?")
	stmtFortuneSelect, _ = db.Prepare("SELECT id, message FROM Fortune")
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#json-serialization
func jsonSerialization(c *fiber.Ctx) {
	c.Json(JSON{"Hello, World!"})
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#single-database-query
func singleDatabaseQuery(c *fiber.Ctx) {
	var json World
	stmtWorldSelect.QueryRow(rand.Intn(10000)+1).Scan(&json.Id, &json.RandomNumber)
	c.Json(json)
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#multiple-database-queries
func multipleDatabaseQueries(c *fiber.Ctx) {
	n, _ := strconv.Atoi(c.Query("queries"))
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		stmtWorldSelect.QueryRow(rand.Intn(10000)+1).Scan(&w.Id, &w.RandomNumber)
	}
	c.Json(Worlds(worlds))
}

// https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
func databaseUpdates(c *fiber.Ctx) {
	n, _ := strconv.Atoi(c.Query("queries"))
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		stmtWorldSelect.QueryRow(rand.Intn(10000)+1).Scan(&w.Id, &w.RandomNumber)
		w.RandomNumber = int32(rand.Intn(10000) + 1)
	}

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
	c.SendString("Hello, World!")
}
