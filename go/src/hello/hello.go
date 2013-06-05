package main

import (
	"database/sql"
	"encoding/json"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"

	_ "github.com/go-sql-driver/mysql"
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

// TODO: remove ?charset=utf8 drom DSN after the next Go-MySQL-Driver release
// https://github.com/go-sql-driver/mysql#unicode-support
const (
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world?charset=utf8"
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256
)

var (
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt
)

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

	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/queries", queriesHandler)
	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/fortune", fortuneHandler)
	http.HandleFunc("/update", updateHandler)
	http.HandleFunc("/plaintext", plaintextHandler)
	http.ListenAndServe(":8080", nil)
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	json.NewEncoder(w).Encode(&Message{"Hello, world"})
}

var HelloWorld = []byte("Hello, World!")

func plaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Write(HelloWorld)
}

// Test 2: Single database query
func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&world)
}

// Test 3: Multiple database queries
func queriesHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n <= 1 {
		dbHandler(w, r)
		return
	}

	world := make([]World, n)
	for i := 0; i < n; i++ {
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(world)
}

// Test 4: Fortunes
func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	rows, err := fortuneStatement.Query()
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
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	w.Header().Set("Content-Type", "text/html")
	if err := tmpl.Execute(w, fortunes); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database updates
func updateHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	var jsonData []byte

	if n <= 1 {
		var world World
		worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
		world.RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		updateStatement.Exec(world.RandomNumber, world.Id)
		jsonData, _ = json.Marshal(world)
	} else {
		world := make([]World, n)
		for i := 0; i < n; i++ {
			if err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
				log.Fatalf("Error scanning world row: %s", err.Error())
			}
			world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
			if _, err := updateStatement.Exec(world[i].RandomNumber, world[i].Id); err != nil {
				log.Fatalf("Error updating world row: %s", err.Error())
			}
		}
		jsonData, _ = json.Marshal(world)
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Content-Length", strconv.Itoa(len(jsonData)))
	w.Write(jsonData)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
