package main

import (
	"database/sql"
	"log"
	"math"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"
	"strings"

	"github.com/go-martini/martini"
	_ "github.com/go-sql-driver/mysql"
	"github.com/martini-contrib/render"
)

const (
	helloWorld  = "Hello, World!"
	contentType = "Content-Type"
	textPlain   = "text/plain"
	newFortune  = "Foo is in a bar. Baz."
	server      = "Server"
	serverValue = "Go with Martini"

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
	db           *sql.DB
	err          error
	stmtWorld    *sql.Stmt
	stmtFortunes *sql.Stmt
	stmtUpdate   *sql.Stmt
)

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	db, err = sql.Open("mysql", mySQLConnection)
	if err != nil {
		log.Fatalf("Error opening connection to MySQL: %v.\n", err)
	}
	db.SetMaxIdleConns(maxIdleConns)
	if stmtWorld, err = db.Prepare(selectWorld); err != nil {
		log.Fatal("Could not create statement for selecting from world.\n", err)
	}
	if stmtFortunes, err = db.Prepare(selectFortunes); err != nil {
		log.Fatal("Could not create statement for selecting a fortune.\n", err)
	}
	if stmtUpdate, err = db.Prepare(updateWorld); err != nil {
		log.Fatal("Could not create statement for updating world.\n", err)
	}

	martini.Env = martini.Prod

	http.ListenAndServe(":8080", makeMartini())
}

// Make a custom martini, injecting two middlewares:
//   - a rendererer to easily output JSON and HTML
//   - one which correctly set the `Server` HTTP header
// avoid the logging middleware to be fair with other frameworks
func makeMartini() *martini.Martini {
	m := martini.New()
	m.Handlers(
		render.Renderer(),
		func(res http.ResponseWriter) {
			res.Header().Set(server, serverValue)
		},
	)

	r := martini.NewRouter()
	r.Get("/json", handleJson)
	r.Get("/db", handleSimpleQuery)
	r.Get("/queries", handleMultipleQueries)
	r.Get("/updates", handleUpdates)
	r.Get("/fortunes", handleFortunes)
	r.Get("/plaintext", handlePlainText)

	m.MapTo(r, (*martini.Routes)(nil))
	m.Action(r.Handle)
	return m
}

// test 1: Simple Hello World JSON response
func handleJson(r render.Render) {
	r.JSON(200, &Hello{helloWorld})
}

// test 2: Random select in MySQL
func handleSimpleQuery(r render.Render) {
	var world World
	selectRandomWorld(&world)
	r.JSON(200, &world)
}

// test 3: Multiple select in MySQL
func handleMultipleQueries(r render.Render, req *http.Request) {
	n := queries(req)
	if n == 1 {
		var world World
		selectRandomWorld(&world)
		r.JSON(200, &world)
	} else {
		worlds := make([]World, n)
		for i := 0; i < len(worlds); i++ {
			selectRandomWorld(&worlds[i])
		}
		r.JSON(200, &worlds)
	}
}

// test 4: Retrieve all fortunes
func handleFortunes(res http.ResponseWriter, r render.Render) {
	rows, err := stmtFortunes.Query()
	if err != nil {
		log.Fatalf("Error selecting fortunes: %s.\n", err.Error)
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() {
		var fortune Fortune
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error reading fortune: %s.\n", err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	// add one at runtime then sort
	fortunes = append(fortunes, &Fortune{Message: newFortune})
	sort.Sort(FortuneMessageComparator{fortunes})

	r.HTML(200, "fortunes", &fortunes)
}

// test 5: Updates
func handleUpdates(r render.Render, req *http.Request) {
	worlds := make([]World, queries(req))
	for i := 0; i < len(worlds); i++ {
		selectRandomWorld(&worlds[i])
		newRandomNumber := uint16(rand.Intn(worldRowCount) + 1)
		if _, err := stmtUpdate.Exec(worlds[i].RandomNumber, worlds[i].Id); err != nil {
			log.Fatalf("Error while updating a world: %s.\n", err.Error())
		}
		worlds[i].RandomNumber = newRandomNumber
	}

	if len(worlds) == 1 {
		r.JSON(200, &worlds[0])
	} else {
		r.JSON(200, &worlds)
	}
}

// test 6: Plain text
func handlePlainText(res http.ResponseWriter) string {
	res.Header().Set(contentType, textPlain)
	return helloWorld
}

// Read a row from `world`
func selectRandomWorld(world *World) {
	if err := stmtWorld.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber); err != nil {
		log.Fatalf("Error reading from world: %s.\n", err.Error())
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
