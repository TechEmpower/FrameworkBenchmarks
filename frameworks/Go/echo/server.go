package main

import (
	"database/sql"
	"encoding/json"
	"html/template"
	"io"
	"log"
	"math/rand"
	"net/http"
	"sort"
	"strconv"

	_ "github.com/go-sql-driver/mysql"
	"github.com/labstack/echo"
	"github.com/labstack/echo/engine/fasthttp"
)

const (
	// Database
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world"
	worldRowCount      = 10000
	macIdleConnection  = 30
	maxConnectionCount = 256

	helloWorldString = "Hello, World!"
	worldSelect      = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate      = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect    = "SELECT id, message FROM Fortune;"
)

var (
	// Database
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt
)

type Template struct {
	templates *template.Template
}

type MessageStruct struct {
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

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }

// Render HTML
func (t *Template) Render(w io.Writer, name string, data interface{}, _ echo.Context) error {
	return t.templates.ExecuteTemplate(w, name, data)
}

// queries parameter between 1 and 500.
func sanitizeQueryParam(param string) int {
	if param == "" { // shortcut
		return 1
	}
	queries, err := strconv.Atoi(param)
	if err != nil || queries < 1 {
		return 1
	}
	if queries > 500 {
		return 500
	}
	return queries
}

// RenderJSON output JSON.
//
// Normally, c.JSON() should be used.  But it's mimetype contains charset.
// In TFB, `Content-Type: application/json` is preferred.
func RenderJSON(c echo.Context, code int, i interface{}) (err error) {
	b, err := json.Marshal(i)
	if err != nil {
		return
	}
	res := c.Response()
	res.Header().Set(echo.ContentType, echo.ApplicationJSON)
	res.WriteHeader(code)
	_, err = res.Write(b)
	return
}

// RenderJSON output JSON without charset in mimetype.
func RenderString(c echo.Context, code int, s string) (err error) {
	res := c.Response()
	res.Header().Set(echo.ContentType, echo.TextPlain)
	res.WriteHeader(code)
	_, err = res.Write([]byte(s))
	return
}

func handleJSON(c echo.Context) error {
	return RenderJSON(c, http.StatusOK, MessageStruct{"Hello, World!"})
}

func plaintext(c echo.Context) error {
	return RenderString(c, http.StatusOK, "Hello, World!")
}

func fortunes(c echo.Context) error {
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
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	return c.Render(http.StatusOK, "fortune.html", fortunes)
}

func singleQuery(c echo.Context) error {
	world := World{}
	if err := randomRow().Scan(&world.Id, &world.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}
	return RenderJSON(c, http.StatusOK, world)
}

func multipleQueries(c echo.Context) error {
	// Get Param
	queries := sanitizeQueryParam(c.Param("queries"))
	worlds := make([]World, queries)
	for i := 0; i < queries; i++ {
		if err := randomRow().Scan(&worlds[i].Id, &worlds[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}
	}
	return RenderJSON(c, http.StatusOK, worlds)
}

func updates(c echo.Context) error {
	// Get Param
	queries := sanitizeQueryParam(c.Param("queries"))
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
	return RenderJSON(c, http.StatusOK, worlds)
}

func main() {
	e := echo.New()

	// Set Templates
	tmpl := &Template{
		templates: template.Must(template.ParseFiles("fortune.html", "layout.html")),
	}
	e.SetRenderer(tmpl)

	// Routes
	e.Get("/json", echo.HandlerFunc(handleJSON))
	e.Get("/db", echo.HandlerFunc(singleQuery))
	e.Get("/queries/", echo.HandlerFunc(multipleQueries))
	e.Get("/queries/:queries", echo.HandlerFunc(multipleQueries))
	e.Get("/fortunes", echo.HandlerFunc(fortunes))
	e.Get("/updates/", echo.HandlerFunc(updates))
	e.Get("/updates/:queries", echo.HandlerFunc(updates))
	e.Get("/plaintext", echo.HandlerFunc(plaintext))

	// Start server
	e.Run(fasthttp.New(":8080"))
}

func init() {
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
}
