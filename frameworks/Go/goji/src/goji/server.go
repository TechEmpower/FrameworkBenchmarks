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
	"strconv"
	"html/template"
	"sort"

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
	extraFortuneMessage = "Additional fortune added at request time."
)

var (
	// Templates
	tmpl = template.Must(template.
		ParseFiles("templates/layout.html",
			       "templates/fortune.html"))

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

func randomRow() *sql.Row {
	return worldStatement.QueryRow(rand.Intn(worldRowCount) + 1)
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

type Fortunes []*Fortune

func (s Fortunes) Len() int {
	return len(s)
}

func (s Fortunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

type ByMessage struct {
	Fortunes
}

func (s ByMessage) Less(i, j int) bool {
	return s.Fortunes[i].Message < s.Fortunes[j].Message
}

// Test 1: Json Serialization
func serializeJson(c web.C, w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(&Message{helloWorldString})
}

// Test 2: Single Database Query
func singleQuery(c web.C, w http.ResponseWriter, r *http.Request) {
	world := World{}
	if err := randomRow().Scan(&world.Id, &world.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&world)
}

// Test 3: Multiple Database Queries
func multipleQueries(c web.C, w http.ResponseWriter, r *http.Request) {
	n := 1
	if queries := r.URL.Query().Get("queries"); len(queries) > 0 {
		if conv, err := strconv.Atoi(queries); err != nil {
			n = 1
		} else {
			n = conv
		}
	}

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		if err := randomRow().Scan(&worlds[i].Id, &worlds[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(worlds)
}

// Test 4: Fortunes
func fortunes(c web.C, w http.ResponseWriter, r *http.Request) {
	rows, err := fortuneStatement.Query()
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}

	fortunes := make(Fortunes, 0, 16)

	for rows.Next() {
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: extraFortuneMessage})

	sort.Sort(ByMessage{fortunes})
	w.Header().Set("Content-Type", "text/html")
	if err := tmpl.Execute(w, fortunes); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
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
	goji.Get("/queries", multipleQueries)
	goji.Get("/fortunes", fortunes)
	goji.Get("/plaintext", plaintext)
	goji.Serve()
}
