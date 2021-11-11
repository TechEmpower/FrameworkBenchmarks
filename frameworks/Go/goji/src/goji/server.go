package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"fmt"
	"html/template"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"

	_ "github.com/go-sql-driver/mysql"

	"github.com/zenazn/goji"
	"github.com/zenazn/goji/web"
)

const (
	// Database
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world"
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256

	helloWorldString    = "Hello, World!"
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

// Sets the content type of response. Also adds the Server header.
func setContentType(w http.ResponseWriter, contentType string) {
	w.Header().Set("Server", "Goji")
	w.Header().Set("Content-Type", contentType)
}

// Test 1: Json Serialization
func serializeJson(c web.C, w http.ResponseWriter, r *http.Request) {
	setContentType(w, "application/json")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}

// Test 2: Single Database Query
func singleQuery(c web.C, w http.ResponseWriter, r *http.Request) {
	world := World{}
	if err := randomRow().Scan(&world.Id, &world.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}

	setContentType(w, "application/json")
	json.NewEncoder(w).Encode(&world)
}

// Caps queries parameter between 1 and 500.
// Non-int values like "foo" and "" become 1.
func sanitizeQueryParam(queries string) int {
	n := 1
	if len(queries) > 0 {
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
	return n
}

// Test 3: Multiple Database Queries
func multipleQueries(c web.C, w http.ResponseWriter, r *http.Request) {
	queries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make([]World, queries)

	for i := 0; i < queries; i++ {
		if err := randomRow().Scan(&worlds[i].Id, &worlds[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
	}

	setContentType(w, "application/json")
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
	setContentType(w, "text/html;charset=utf-8")
	if err := tmpl.Execute(w, fortunes); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database Updates
func dbupdate(c web.C, w http.ResponseWriter, r *http.Request) {
	queries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make([]World, queries)

	for i := 0; i < queries; i++ {
		if err := randomRow().Scan(&worlds[i].Id, &worlds[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
		worlds[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := updateStatement.Exec(worlds[i].RandomNumber, worlds[i].Id); err != nil {
			log.Fatalf("Error updating world row: %s", err.Error())
		}
	}

	setContentType(w, "application/json")
	json.NewEncoder(w).Encode(worlds)
}

// Test 6: Plaintext
func plaintext(c web.C, w http.ResponseWriter, r *http.Request) {
	setContentType(w, "text/plain")
	fmt.Fprintf(w, helloWorldString)
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	log.SetOutput(ioutil.Discard) // add line 3
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
	goji.Get("/updates", dbupdate)
	goji.Serve()
}
