package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"sort"
	"strconv"

	"golang.org/x/net/context"

	_ "github.com/go-sql-driver/mysql"
	"github.com/guregu/kami"
)

type Message struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

// Databases
const (
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world?interpolateParams=true"
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256
)

const helloWorldString = "Hello, World!"

var (
	// Templates
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	// Database
	db *sql.DB

	helloWorldBytes = []byte(helloWorldString)
)

func main() {
	var err error
	db, err = sql.Open("mysql", connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnectionCount)

	flag.Set("bind", ":8080")

	kami.Use("/", serverMiddleware)
	kami.Get("/json", jsonHandler)
	kami.Get("/db", dbHandler)
	kami.Get("/queries", queriesHandler)
	kami.Get("/fortunes", fortuneHandler)
	kami.Get("/updates", updateHandler)
	kami.Get("/plaintext", plaintextHandler)
	kami.Serve()
}

// serverMiddleware will set the Server header on all outgoing requests
func serverMiddleware(ctx context.Context, w http.ResponseWriter, _ *http.Request) context.Context {
	w.Header().Set("Server", "kami")
	return ctx
}

// jsonHandler implements Test 1: JSON Serializer
func jsonHandler(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}

// Test 2: Single database query
func dbHandler(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	var world World
	err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&world)
}

// Test 3: Multiple database queries
func queriesHandler(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	world := make([]World, n)
	for i := 0; i < n; i++ {
		err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(world)
}

// Test 4: Fortunes
func fortuneHandler(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	rows, err := db.Query(fortuneSelect)
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { //Fetch rows
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	rows.Close()
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	w.Header().Set("Content-Type", "text/html;charset=utf-8")
	if err := tmpl.Execute(w, fortunes); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database updates
func updateHandler(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	w.Header().Set("Content-Type", "application/json")
	encoder := json.NewEncoder(w)

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	world := make([]World, n)
	for i := 0; i < n; i++ {
		if err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
		world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := db.Exec(worldUpdate, world[i].RandomNumber, world[i].Id); err != nil {
			log.Fatalf("Error updating world row: %s", err.Error())
		}
	}
	encoder.Encode(world)
}

// Test 6: Plaintext
func plaintextHandler(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Write(helloWorldBytes)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
