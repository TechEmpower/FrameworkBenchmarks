package main

import (
	"encoding/json"
	"flag"
	"html/template"
	"io"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"
	"sort"
	"strconv"

	"github.com/go-chi/chi"
	"github.com/jackc/pgx"
)

const (
	// Database
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdate        = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256

	helloWorldString    = "Hello, World!"
	extraFortuneMessage = "Additional fortune added at request time."
)

var (
	connectionString = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?sslmode=disable"
	bindHost         = ":8080"
	debug            = false

	// Templates
	tmpl = template.Must(template.
		ParseFiles("templates/layout.html",
			"templates/fortune.html"))

	// Database
	helloWorldMessage = &Message{helloWorldString}
	db                *pgx.ConnPool
)

// Message is a JSON struct to render a message
type Message struct {
	Message string `json:"message"`
}

// World is a JSON struct to render a random number
type World struct {
	ID           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

func randomRow() *pgx.Row {
	return db.QueryRow("worldSelect", rand.Intn(worldRowCount)+1)
}

// Fortune renders a fortune in JSON
type Fortune struct {
	ID      uint16 `json:"id"`
	Message string `json:"message"`
}

// Fortunes is a list of fortunes
type Fortunes []*Fortune

// Len returns the length of the fortunes list
func (s Fortunes) Len() int {
	return len(s)
}

// Swap swaps fortunes at the given positions
func (s Fortunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

// Sets the content type of response. Also adds the Server header.
func setContentType(w http.ResponseWriter, contentType string) {
	w.Header().Set("Server", "Chi")
	w.Header().Set("Content-Type", contentType)
}

// Test 1: JSON Serialization
func serializeJSON(w http.ResponseWriter, r *http.Request) {
	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(helloWorldMessage)
}

// Test 2: Single Database Query
func singleQuery(w http.ResponseWriter, r *http.Request) {
	world := World{}
	if err := randomRow().Scan(&world.ID, &world.RandomNumber); err != nil {
		log.Printf("Error scanning world row: %s", err.Error())
	}

	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(&world)
}

// Caps queries parameter between 1 and 500.
// Non-int values like "foo" and "" become 1.
func sanitizeQueryParam(queries string) int {
	n, _ := strconv.Atoi(queries)

	if n <= 0 {
		return 1
	}

	if n > 500 {
		return 500
	}

	return n
}

// Test 3: Multiple Database Queries
func multipleQueries(w http.ResponseWriter, r *http.Request) {
	queries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make([]World, queries)

	for i := 0; i < queries; i++ {
		if err := randomRow().Scan(&worlds[i].ID, &worlds[i].RandomNumber); err != nil {
			log.Printf("Error scanning world row: %s", err.Error())
		}
	}

	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(worlds)
}

// Test 4: Fortunes
func fortunes(w http.ResponseWriter, r *http.Request) {
	rows, err := db.Query("fortuneSelect")
	if err != nil {
		log.Printf("Error preparing statement: %v", err)
	}

	fortunes := make(Fortunes, 0, 16)

	for rows.Next() {
		fortune := Fortune{}
		if err := rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Printf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	rows.Close()
	fortunes = append(fortunes, &Fortune{Message: extraFortuneMessage})

	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})
	setContentType(w, "text/html")
	if err := tmpl.Execute(w, fortunes); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database Updates
func dbupdate(w http.ResponseWriter, r *http.Request) {
	queries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make([]World, queries)

	for i := 0; i < queries; i++ {
		w := &worlds[i]
		if err := randomRow().Scan(&w.ID, &w.RandomNumber); err != nil {
			log.Printf("Error scanning world row: %s", err.Error())
		}
		worlds[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := db.Exec("worldUpdate", w.RandomNumber, w.ID); err != nil {
			log.Printf("Error updating world row: %s", err.Error())
		}
	}

	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(worlds)
}

// Test 6: Plaintext
func plaintext(w http.ResponseWriter, r *http.Request) {
	setContentType(w, "text/plain")
	_, _ = io.WriteString(w, helloWorldString)
}

func init() {
	flag.StringVar(&bindHost, "bind", bindHost, "Set bind host")
	flag.StringVar(&connectionString, "db", connectionString, "Set database URL")
	flag.BoolVar(&debug, "debug", false, "Enable debug mode")
	flag.Parse()
}

func main() {
	if !debug {
		log.SetOutput(ioutil.Discard)
	}

	config, err := pgx.ParseConnectionString(connectionString)
	if err != nil {
		flag.PrintDefaults()
		log.Fatalf("Error parsing db URL: %v", err)
	}

	connPoolConfig := pgx.ConnPoolConfig{
		ConnConfig:     config,
		MaxConnections: maxConnectionCount,
	}

	db, err = pgx.NewConnPool(connPoolConfig)
	if err != nil {
		flag.PrintDefaults()
		log.Fatalf("Error opening database: %v", err)
	}

	_, err = db.Prepare("worldSelect", worldSelect)
	if err != nil {
		flag.PrintDefaults()
		log.Fatal(err)
	}
	_, err = db.Prepare("fortuneSelect", fortuneSelect)
	if err != nil {
		flag.PrintDefaults()
		log.Fatal(err)
	}
	_, err = db.Prepare("worldUpdate", worldUpdate)
	if err != nil {
		flag.PrintDefaults()
		log.Fatal(err)
	}

	r := chi.NewRouter()

	r.Get("/json", serializeJSON)
	r.Get("/db", singleQuery)
	r.Get("/queries", multipleQueries)
	r.Get("/fortunes", fortunes)
	r.Get("/plaintext", plaintext)
	r.Get("/updates", dbupdate)

	err = http.ListenAndServe(bindHost, r)
	if err != nil {
		log.Fatal(err)
	}
}
