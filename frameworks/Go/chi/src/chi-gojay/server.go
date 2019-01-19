package main

import (
	"flag"
	"fmt"
	"html"
	"io"
	"io/ioutil"
	"log"
	"math/rand"
	"net"
	"net/http"
	"sort"
	"strconv"
	"strings"

	"github.com/francoispqt/gojay"
	"github.com/go-chi/chi"
	"github.com/jackc/pgx"
)

const (
	htmlTemplate = `<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body><table><tr><th>id</th><th>message</th></tr>%s</table></body>
</html>`
	fortuneTemplate = `<tr><td>%d</td><td>%s</td></tr>`
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
	debugFlag        = false
	preforkFlag      = false
	childFlag        = false

	helloWorldMessage = &Message{helloWorldString}
	extraFortune      = &Fortune{Message: extraFortuneMessage}
)

// Message is a JSON struct to render a message
type Message struct {
	Message string
}

// MarshalJSONObject encodes the message as JSON
func (m *Message) MarshalJSONObject(dec *gojay.Encoder) {
	dec.AddStringKey("message", m.Message)
}

// IsNil returns true if the object is nil
func (m *Message) IsNil() bool {
	return m == nil
}

// World is a JSON struct to render a random number
type World struct {
	ID           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

// Worlds is a list of World
type Worlds []World

// MarshalJSONArray marshals the list of worlds
func (ws Worlds) MarshalJSONArray(enc *gojay.Encoder) {
	for _, w := range ws {
		enc.AddObject(&w)
	}
}

// IsNil returns true if the object is nil
func (ws Worlds) IsNil() bool {
	return ws == nil
}

// MarshalJSONObject encodes the message as JSON
func (w *World) MarshalJSONObject(dec *gojay.Encoder) {
	dec.AddIntKey("id", int(w.ID))
	dec.AddIntKey("randomNumber", int(w.RandomNumber))
}

// IsNil returns true if the object is nil
func (w *World) IsNil() bool {
	return w == nil
}

func randomRow() *pgx.Row {
	return defaultDB.QueryRow("worldSelect", rand.Intn(worldRowCount)+1)
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

	_ = gojay.NewEncoder(w).Encode(helloWorldMessage)
}

// Test 2: Single Database Query
func singleQuery(w http.ResponseWriter, r *http.Request) {
	world := World{}
	if err := randomRow().Scan(&world.ID, &world.RandomNumber); err != nil {
		log.Printf("Error scanning world row: %s", err.Error())
	}

	setContentType(w, "application/json")
	_ = gojay.NewEncoder(w).Encode(&world)
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
	worlds := make(Worlds, queries)

	for i := 0; i < queries; i++ {
		if err := randomRow().Scan(&worlds[i].ID, &worlds[i].RandomNumber); err != nil {
			log.Printf("Error scanning world row: %s", err.Error())
		}
	}

	setContentType(w, "application/json")
	_ = gojay.NewEncoder(w).EncodeArray(worlds)
}

// Test 4: Fortunes
func fortunes(w http.ResponseWriter, r *http.Request) {
	rows, err := defaultDB.Query("fortuneSelect")
	if err != nil {
		log.Printf("Error preparing statement: %v", err)
	}

	fortunes := make(Fortunes, 0, 256)

	for rows.Next() {
		fortune := Fortune{}
		if err := rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Printf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	rows.Close()
	fortunes = append(fortunes, extraFortune)

	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})
	setContentType(w, "text/html; charset=utf-8")

	var body strings.Builder
	for _, fortune := range fortunes {
		fmt.Fprintf(&body, fortuneTemplate, fortune.ID, html.EscapeString(fortune.Message))
	}

	fmt.Fprintf(w, htmlTemplate, body.String())
}

// Test 5: Database Updates
func dbupdate(w http.ResponseWriter, r *http.Request) {
	queries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make(Worlds, queries)

	for i := 0; i < queries; i++ {
		w := &worlds[i]
		if err := randomRow().Scan(&w.ID, &w.RandomNumber); err != nil {
			log.Printf("Error scanning world row: %s", err.Error())
		}
		worlds[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := defaultDB.Exec("worldUpdate", w.RandomNumber, w.ID); err != nil {
			log.Printf("Error updating world row: %s", err.Error())
		}
	}

	setContentType(w, "application/json")
	_ = gojay.NewEncoder(w).EncodeArray(worlds)
}

// Test 6: Plaintext
func plaintext(w http.ResponseWriter, r *http.Request) {
	setContentType(w, "text/plain")
	_, _ = io.WriteString(w, helloWorldString)
}

func init() {
	flag.StringVar(&bindHost, "bind", bindHost, "Set bind host")
	flag.StringVar(&connectionString, "db", connectionString, "Set database URL")
	flag.BoolVar(&debugFlag, "debug", false, "Enable debug mode")
	flag.BoolVar(&preforkFlag, "prefork", false, "Enable prefork mode")
	flag.BoolVar(&childFlag, "child", false, "Enable child mode")
	flag.Parse()
}

func initRouter() http.Handler {
	r := chi.NewRouter()

	r.Get("/json", serializeJSON)
	r.Get("/db", singleQuery)
	r.Get("/queries", multipleQueries)
	r.Get("/fortunes", fortunes)
	r.Get("/plaintext", plaintext)
	r.Get("/updates", dbupdate)

	return r
}

func startListening(listener net.Listener) error {
	var err error
	if !preforkFlag {
		err = http.ListenAndServe(bindHost, initRouter())
	} else {
		err = http.Serve(listener, initRouter())
	}

	return err
}

func main() {
	var listener net.Listener
	if preforkFlag {
		listener = doPrefork(childFlag, bindHost)
	}

	initDBConnection(connectionString)

	if !debugFlag {
		log.SetOutput(ioutil.Discard)
	}

	if err := startListening(listener); err != nil {
		log.Fatal(err)
	}
}
