package main

import (
	"fmt"
	"net/http"
	"encoding/json"
	"flag"
	"runtime"
	"database/sql"
	"log"
	"math/rand"

	_ "github.com/go-sql-driver/mysql"

	"github.com/zenazn/goji"
	"github.com/zenazn/goji/web"
)

const (
	// Database
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world"
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256

	helloWorldString = "Hello, World!"
)

var (
	// Templates
	// tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	// Database
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt

	// helloWorldBytes = []byte(helloWorldString)
)

type Message struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

// Test 1: Json Serialization
func serializeJson(c web.C, w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(&Message{helloWorldString})
}

// Test 2: Single Database Query
func singleQuery(c web.C, w http.ResponseWriter, r *http.Request) {
	var world World
	err := worldStatement.
	    QueryRow(rand.Intn(worldRowCount)+1).
	    Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&world)
}

// Test 6: Plaintext
func plaintext(c web.C, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World!")
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())

	db, err := sql.Open("mysql", connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnectionCount)
	worldStatement, err = db.Prepare(worldSelect)
	if err != nil {
		log.Fatal(err)
	}
	fortuneStatement, err = db.Prepare(fortuneSelect)
	if err != nil {
		log.Fatal(err)
	}
	updateStatement, err = db.Prepare(worldUpdate)
	if err != nil {
		log.Fatal(err)
	}

	flag.Set("bind", ":8080")
	goji.Get("/json", serializeJson)
	goji.Get("/db", singleQuery)
	goji.Get("/plaintext", plaintext)
	goji.Serve()
}
