package main

import (
	"database/sql"
	"github.com/gorail/core"
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
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	// Database
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt

	helloWorldBytes = []byte(helloWorldString)
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

	app := core.NewApp()

	app.DefaultRouter = app.DirRouter("main").RegisterFuncMap(core.FuncMap{
		"db":        dbHandler,
		"queries":   queriesHandler,
		"json":      jsonHandler,
		"fortune":   fortuneHandler,
		"update":    updateHandler,
		"plaintext": plaintextHandler,
	})

	app.Listen(":8080")
}

// Test 1: JSON serialization
func jsonHandler(c *core.Context) {
	c.Res.Header().Set("Content-Type", "application/javascript")
	c.Json().Send(&Message{helloWorldString})
}

// Test 2: Single database query
func dbHandler(c *core.Context) {
	var world World
	err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}

	c.Res.Header().Set("Content-Type", "application/json")
	c.Json().Send(&world)
}

// Test 3: Multiple database queries
func queriesHandler(c *core.Context) {
	n := 1
	if nStr := c.Req.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n <= 1 {
		dbHandler(c)
		return
	}

	world := make([]World, n)
	for i := 0; i < n; i++ {
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
	}

	c.Res.Header().Set("Content-Type", "application/json")
	c.Json().Send(world)
}

// Test 4: Fortunes
func fortuneHandler(c *core.Context) {
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
	c.Res.Header().Set("Content-Type", "text/html")
	if err := tmpl.Execute(c.Res, fortunes); err != nil {
		http.Error(c.Res, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database updates
func updateHandler(c *core.Context) {
	n := 1
	if nStr := c.Req.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	c.Res.Header().Set("Content-Type", "application/json")

	if n <= 1 {
		var world World
		worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
		world.RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		updateStatement.Exec(world.RandomNumber, world.Id)
		c.Json().Send(&world)
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
		c.Json().Send(world)
	}
}

// Test 6: Plaintext
func plaintextHandler(c *core.Context) {
	c.Res.Header().Set("Content-Type", "text/plain")
	c.Res.Write(helloWorldBytes)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
