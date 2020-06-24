package main

import (
	"database/sql"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"

	"clevergo.tech/clevergo"
	_ "github.com/go-sql-driver/mysql"
)

type Fortune struct {
	ID      uint16 `json:"id"`
	Message string `json:"message"`
}

type Fortunes []*Fortune

func (s Fortunes) Len() int {
	return len(s)
}

func (s Fortunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

type FortunesByMessage struct {
	Fortunes
}

func (f FortunesByMessage) Less(i, j int) bool {
	return f.Fortunes[i].Message < f.Fortunes[j].Message
}

var (
	// Template
	fortuneHTML = `<!doctype html>
<html>
<head>
	<title>Fortunes</title>
</head>
<body>
	<table>
		<tr>
        	<th>id</th>
        	<th>message</th>
        </tr>
        {{range .}}
        <tr>
        	<td>{{ .ID }}</td>
        	<td>{{ .Message }}</td>
        </tr>
        {{end}}
	</table>
</body>
</html>`
	fortuneTmpl = template.Must(template.New("fortune").Parse(fortuneHTML))

	// Database
	db                *sql.DB
	dbDSN             = "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world"
	worldSelect       = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate       = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect     = "SELECT id, message FROM Fortune"
	worldSelectStmt   *sql.Stmt
	worldUpdateStmt   *sql.Stmt
	fortuneSelectStmt *sql.Stmt
	worldRowCount     = 10000
	maxConnections    = 256

	helloWorld = "Hello, World!"
)

func init() {
	runtime.GOMAXPROCS(runtime.NumCPU())
}

func initialize() (err error) {
	db, err = sql.Open("mysql", dbDSN)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnections)
	db.SetMaxOpenConns(maxConnections)
	worldSelectStmt, err = db.Prepare(worldSelect)
	if err != nil {
		log.Fatal(err)
	}
	worldUpdateStmt, err = db.Prepare(worldUpdate)
	if err != nil {
		log.Fatal(err)
	}
	fortuneSelectStmt, err = db.Prepare(fortuneSelect)
	if err != nil {
		log.Fatal(err)
	}

	return
}

func main() {
	if err := initialize(); err != nil {
		log.Fatal(err)
	}

	app := clevergo.Pure()
	app.Use(clevergo.ServerHeader("CleverGo"))
	app.Get("/plaintext", plaintextHandler)
	app.Get("/json", jsonHandler)
	app.Get("/db", dbHandler)
	app.Get("/queries", queriesHandler)
	app.Get("/fortunes", fortunesHandler)
	app.Get("/updates", updateHandler)
	log.Println(app.Run(":8080"))
}

func dbHandler(c *clevergo.Context) error {
	world := new(World)
	if err := fetchRandomWorld(world); err != nil {
		return err
	}
	return c.JSON(http.StatusOK, world)
}

func getQueryCount(q string) int {
	n, err := strconv.Atoi(q)
	if err != nil || n < 1 {
		return 1
	}
	if n > 500 {
		return 500
	}
	return n
}

type World struct {
	ID           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

func fetchRandomWorld(w *World) error {
	n := randomWorldNum()
	return worldSelectStmt.QueryRow(n).Scan(&w.ID, &w.RandomNumber)
}

func randomWorldNum() int {
	return rand.Intn(worldRowCount) + 1
}

func queriesHandler(c *clevergo.Context) error {
	n := getQueryCount(c.QueryParam("n"))
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		if err := fetchRandomWorld(&worlds[i]); err != nil {
			return err
		}
	}

	return c.JSON(http.StatusOK, worlds)
}

func fortunesHandler(c *clevergo.Context) error {
	rows, err := fortuneSelectStmt.Query()
	if err != nil {
		return err
	}
	defer rows.Close()

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { // Fetch rows
		f := new(Fortune)
		if err := rows.Scan(&f.ID, &f.Message); err != nil {
			return err
		}
		fortunes = append(fortunes, f)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(FortunesByMessage{fortunes})

	return fortuneTmpl.Execute(c.Response, fortunes)
}

func updateHandler(c *clevergo.Context) error {
	n := getQueryCount(c.QueryParam("n"))
	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		// Fetch and modify
		w := &worlds[i]
		if err := fetchRandomWorld(&worlds[i]); err != nil {
			return err
		}
		w.RandomNumber = uint16(randomWorldNum())

		// Update
		if _, err := worldUpdateStmt.Exec(w.RandomNumber, w.ID); err != nil {
			return err
		}
	}

	return c.JSON(http.StatusOK, worlds)
}

func plaintextHandler(c *clevergo.Context) error {
	return c.String(http.StatusOK, helloWorld)
}

type Message struct {
	Message string `json:"message"`
}

func jsonHandler(c *clevergo.Context) error {
	return c.JSON(http.StatusOK, Message{"Hello, World!"})
}
