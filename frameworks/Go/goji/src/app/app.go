package main

import (
	"database/sql"
	"encoding/json"
	"html/template"
	"log"
	"math"
	"math/rand"
	"net/http"
	"os"
	"runtime"
	"sort"
	"strconv"
	"strings"

	_ "github.com/go-sql-driver/mysql"
	"github.com/zenazn/goji"
	"github.com/zenazn/goji/web"
	gojiwebmiddleware "github.com/zenazn/goji/web/middleware"
)

const (
	helloWorld      = "Hello, World!"
	contentType     = "Content-Type"
	applicationJson = "application/json"
	textPlain       = "text/plain"
	textHtml        = "text/html; charset=utf-8"
	newFortune      = "Foo is in a bar. Baz."
	server          = "Server"
	serverValue     = "Go with Goji"

	cQueries      = "queries"
	queriesArgUrl = "?queries="

	mySQLConnection = "benchmarkdbuser:benchmarkdbpass@tcp(127.0.0.1:3306)/hello_world"
	selectWorld     = "select id, randomNumber from world where id = ?"
	updateWorld     = "update world set randomNumber = ? where id = ?"
	selectFortunes  = "select id, message from fortune"

	worldRowCount    = 10000
	maxIdleConns     = 256
	maxQueriesUrlArg = 500.
)

var (
	tpl             = template.Must(template.ParseFiles("templates/fortunes.tmpl"))
	helloWorldBytes = []byte(helloWorld)
	db              *sql.DB
	err             error
	stmtWorld       *sql.Stmt
	stmtFortunes    *sql.Stmt
	stmtUpdate      *sql.Stmt
)

func main() {
	// make Goji run on port 8080
	os.Args = []string{"app", "-bind", ":8080"}

	runtime.GOMAXPROCS(runtime.NumCPU())
	db, err = sql.Open("mysql", mySQLConnection)
	if err != nil {
		log.Fatalf("Error opening connection to MySQL: %v.\n", err)
	}
	db.SetMaxIdleConns(maxIdleConns)
	if stmtWorld, err = db.Prepare(selectWorld); err != nil {
		log.Fatalf("Could not create statement for selecting from world: %v.\n", err)
	}
	if stmtFortunes, err = db.Prepare(selectFortunes); err != nil {
		log.Fatalf("Could not create statement for selecting a fortune: %v.\n", err)
	}
	if stmtUpdate, err = db.Prepare(updateWorld); err != nil {
		log.Fatalf("Could not create statement for updating world: %v.\n", err)
	}

	runGoji()
}

func runGoji() {
	goji.Get("/json", handleJson)
	goji.Get("/db", handleSimpleQuery)
	goji.Get("/queries", handleMultipleQueries)
	goji.Get("/fortunes", handleFortunes)
	goji.Get("/updates", handleUpdates)
	goji.Get("/plaintext", handlePlainText)
	goji.Use(func(h http.Handler) http.Handler {
		handler := func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set(server, serverValue)
			h.ServeHTTP(w, r)
		}
		return http.HandlerFunc(handler)
	})
	goji.Abandon(gojiwebmiddleware.Logger) // no logging

	goji.Serve()
}

func writeJson(w http.ResponseWriter, v interface{}) {
	w.Header().Set(contentType, applicationJson)
	b, err := json.Marshal(&v)
	if err != nil {
		log.Fatalf("Could not write JSON: %v\n", err)
	}
	w.Write(b)
}

// test 1: Simple Hello World JSON response
func handleJson(c web.C, w http.ResponseWriter, r *http.Request) {
	writeJson(w, &Hello{helloWorld})
}

// test 2: Random select in MySQL
func handleSimpleQuery(c web.C, w http.ResponseWriter, r *http.Request) {
	var world World
	selectRandomWorld(&world)
	writeJson(w, &world)
}

// test 3: Multiple select in MySQL
func handleMultipleQueries(c web.C, w http.ResponseWriter, r *http.Request) {
	worlds := make([]World, queries(r))
	for i := 0; i < len(worlds); i++ {
		selectRandomWorld(&worlds[i])
	}
	writeJson(w, &worlds)
}

// test 4: Retrieve all fortunes
func handleFortunes(c web.C, w http.ResponseWriter, r *http.Request) {
	rows, err := stmtFortunes.Query()
	if err != nil {
		log.Fatalf("Error selecting fortunes: %v.\n", err)
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() {
		var fortune Fortune
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error reading fortune: %v.\n", err)
		}
		fortunes = append(fortunes, &fortune)
	}
	// add one at runtime then sort
	fortunes = append(fortunes, &Fortune{Message: newFortune})
	sort.Sort(FortuneMessageComparator{fortunes})

	w.Header().Set(contentType, textHtml)
	if err = tpl.Execute(w, &fortunes); err != nil { // write template into response
		log.Fatalf("Error writing template into response: %v.\n", err)
	}
}

// test 5: Updates
func handleUpdates(c web.C, w http.ResponseWriter, r *http.Request) {
	worlds := make([]World, queries(r))
	for i := 0; i < len(worlds); i++ {
		selectRandomWorld(&worlds[i])
		newRandomNumber := uint16(rand.Intn(worldRowCount) + 1)
		if _, err := stmtUpdate.Exec(worlds[i].RandomNumber, worlds[i].Id); err != nil {
			log.Fatalf("Error while updating a world: %v.\n", err)
		}
		worlds[i].RandomNumber = newRandomNumber
	}

	writeJson(w, &worlds)
}

// test 6: Plain text
func handlePlainText(c web.C, w http.ResponseWriter, r *http.Request) {
	w.Header().Set(contentType, textPlain)
	w.Write(helloWorldBytes)
}

// Read a row from `world`
func selectRandomWorld(world *World) {
	if err := stmtWorld.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber); err != nil {
		log.Fatalf("Error reading from world: %v.\n", err)
	}
}

// Extract the ?queries= from URL, make sure it is between 1 and 500
func queries(req *http.Request) int {
	n := 1
	if strings.Contains(req.URL.String(), queriesArgUrl) {
		if queries := req.URL.Query().Get(cQueries); len(queries) > 0 {
			nQueries, _ := strconv.Atoi(queries)
			n = int(math.Min(math.Max(float64(nQueries), 1.), maxQueriesUrlArg))
		}
	}
	return n
}
