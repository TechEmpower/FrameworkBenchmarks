package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"log"
	"math/rand"
	"net/http"
	"sort"
	"strconv"

	"github.com/labstack/echo/v4"
	_ "github.com/lib/pq"
)

type (
	handler struct{}

	StdTemplate struct {
		templates *template.Template
	}

	Message struct {
		Message string `json:"message"`
	}

	World struct {
		ID           uint16 `json:"id"`
		RandomNumber uint16 `json:"randomNumber"`
	}

	Fortune struct {
		ID      uint16 `json:"id"`
		Message string `json:"message"`
	}

	Fortunes []*Fortune

	FortunesByMessage struct {
		Fortunes
	}
)

const (
	// Template
	fortuneHTML = `
    <!doctype html>
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
    </html>
  `
	// Database
	connectionString = "postgres://benchmarkdbuser:benchmarkdbpass@%s/hello_world?sslmode=disable"
	worldSelect      = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdate      = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelect    = "SELECT id, message FROM Fortune"
	worldRowCount    = 10000
	maxConnections   = 256
)

var (
	// Database
	db                *sql.DB
	worldSelectStmt   *sql.Stmt
	worldUpdateStmt   *sql.Stmt
	fortuneSelectStmt *sql.Stmt

	// Template
	Template = &StdTemplate{
		templates: template.Must(template.New("fortune").Parse(fortuneHTML)),
	}

	helloWorld = []byte("Hello, World!")
)

func (t *StdTemplate) Render(w io.Writer, name string, data interface{}, c echo.Context) error {
	return t.templates.ExecuteTemplate(w, name, data)
}

func (f Fortunes) Len() int {
	return len(f)
}

func (f Fortunes) Swap(i, j int) {
	f[i], f[j] = f[j], f[i]
}

func (f FortunesByMessage) Less(i, j int) bool {
	return f.Fortunes[i].Message < f.Fortunes[j].Message
}

// Test 1: JSON serialization
func (h *handler) json() echo.HandlerFunc {
	return func(c echo.Context) error {
		c.Response().Header().Set("Server", "Echo")
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMEApplicationJSON)
		return json.NewEncoder(c.Response()).Encode(Message{"Hello, World!"})
	}
}

// Test 2: Single database query
func (h *handler) db() echo.HandlerFunc {
	return func(c echo.Context) error {
		world := new(World)
		if err := fetchRandomWorld(world); err != nil {
			return err
		}

		c.Response().Header().Set("Server", "Echo")
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMEApplicationJSON)
		return json.NewEncoder(c.Response()).Encode(world)
	}
}

// Test 3: Multiple database queries
func (h *handler) queries() echo.HandlerFunc {
	return func(c echo.Context) error {
		n := getQueryCount(c.QueryParam("n"))
		worlds := make([]World, n)
		for i := 0; i < n; i++ {
			if err := fetchRandomWorld(&worlds[i]); err != nil {
				return err
			}
		}

		c.Response().Header().Set("Server", "Echo")
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMEApplicationJSON)
		return json.NewEncoder(c.Response()).Encode(worlds)
	}
}

// Test 4: Fortunes
func (h *handler) fortunes() echo.HandlerFunc {
	return func(c echo.Context) error {
		rows, err := fortuneSelectStmt.Query()
		if err != nil {
			return fmt.Errorf("Error preparing statement: %v", err)
		}
		defer rows.Close()

		fortunes, err := fetchFortunes(rows)
		if err != nil {
			return err
		}
		fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
		sort.Sort(FortunesByMessage{fortunes})

		c.Response().Header().Set("Server", "Echo")
		return c.Render(http.StatusOK, "fortune", fortunes)
	}
}

// Test 5: Database updates
func (h *handler) updates() echo.HandlerFunc {
	return func(c echo.Context) error {
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
				return fmt.Errorf("Error updating world row: %v", err)
			}
		}

		// sorting is required for insert deadlock prevention.
		// sort.Sort(WorldsByID(worlds))
		// Update
		// tx, err := db.Begin()
		// if err != nil {
		// 	return fmt.Errorf("Error starting transaction: %s", err)
		// }
		// for i := 0; i < n; i++ {
		// 	w := &worlds[i]
		// 	if _, err = tx.Stmt(worldUpdateStmt).Exec(w.RandomNumber, w.ID); err != nil {
		// 		return fmt.Errorf("Error updating world row %d: %s", i, err)
		// 	}
		// }
		// if err = tx.Commit(); err != nil {
		// 	return fmt.Errorf("Error when commiting world rows: %s", err)
		// }

		c.Response().Header().Set("Server", "Echo")
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMEApplicationJSON)
		return json.NewEncoder(c.Response()).Encode(worlds)
	}
}

// Test 6: Plaintext
func (h *handler) plaintext() echo.HandlerFunc {
	return func(c echo.Context) error {
		c.Response().Header().Set("Server", "Echo")
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
		_, err := c.Response().Write(helloWorld)
		return err
	}
}

func fetchRandomWorld(w *World) error {
	n := randomWorldNum()
	return worldSelectStmt.QueryRow(n).Scan(&w.ID, &w.RandomNumber)
}

func randomWorldNum() int {
	return rand.Intn(worldRowCount) + 1
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

func fetchFortunes(rows *sql.Rows) (Fortunes, error) {
	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { // Fetch rows
		f := new(Fortune)
		if err := rows.Scan(&f.ID, &f.Message); err != nil {
			return nil, fmt.Errorf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, f)
	}
	return fortunes, nil
}

func InitRoutes(e *echo.Echo) {
	h := new(handler)
	e.GET("/json", h.json())
	e.GET("/db", h.db())
	e.GET("/queries", h.queries())
	e.GET("/fortunes", h.fortunes())
	e.GET("/updates", h.updates())
	e.GET("/plaintext", h.plaintext())
}

func InitPostgres() {
	host := "tfb-database"

	var err error
	db, err = sql.Open("postgres", fmt.Sprintf(connectionString, host))
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
}

func main() {
	e := echo.New()
	e.Renderer = Template
	InitRoutes(e)
	InitPostgres()
	e.Start(":8080")
}
