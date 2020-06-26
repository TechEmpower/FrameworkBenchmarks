package main

import (
	"encoding/json"
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

	"github.com/go-chi/chi"
)

const (
	htmlTemplate    = `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>%s</table></body></html>`
	fortuneTemplate = `<tr><td>%d</td><td>%s</td></tr>`

	helloWorldString    = "Hello, World!"
	extraFortuneMessage = "Additional fortune added at request time."
)

var (
	bindHost    = ":8080"
	debugFlag   = false
	preforkFlag = false
	childFlag   = false

	helloWorldMessage = &Message{helloWorldString}
	extraFortune      = &Fortune{Message: extraFortuneMessage}
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

func (s Fortunes) Less(i, j int) bool { return s[i].Message < s[j].Message }

// Sets the content type of response. Also adds the Server header.
func setContentType(w http.ResponseWriter, contentType string) {
	w.Header().Set("Server", "chi")
	w.Header().Set("Content-Type", contentType)
}

// Test 1: JSON Serialization
func serializeJSON(w http.ResponseWriter, r *http.Request) {
	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(helloWorldMessage)
}

// Test 2: Single Database Query
func singleQuery(w http.ResponseWriter, r *http.Request) {
	var world World
	err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.ID, &world.RandomNumber)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(&world)
}

// Caps queries parameter between 1 and 500.
// Non-int values like "foo" and "" become 1.
func sanitizeQueryParam(queries string) int {
	n, err := strconv.Atoi(queries)
	if err != nil {
		n = 1
	} else if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

// Test 3: Multiple Database Queries
func multipleQueries(w http.ResponseWriter, r *http.Request) {
	queries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make([]World, queries)

	for i := 0; i < queries; i++ {
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&worlds[i].ID, &worlds[i].RandomNumber)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}

	setContentType(w, "application/json")
	_ = json.NewEncoder(w).Encode(worlds)
}

// Test 4: Fortunes
func fortunes(w http.ResponseWriter, r *http.Request) {
	rows, err := fortuneStatement.Query()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { //Fetch rows
		fortune := Fortune{}
		if err := rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(fortunes)

	setContentType(w, "text/html; charset=utf-8")

	var body strings.Builder
	for _, fortune := range fortunes {
		fmt.Fprintf(&body, fortuneTemplate, fortune.ID, html.EscapeString(fortune.Message))
	}

	fmt.Fprintf(w, htmlTemplate, body.String())
}

// Test 5: Database Updates
func dbupdate(w http.ResponseWriter, r *http.Request) {
	numQueries := sanitizeQueryParam(r.URL.Query().Get("queries"))
	worlds := make([]World, numQueries)
	for i := 0; i < numQueries; i++ {
		if err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&worlds[i].ID, &worlds[i].RandomNumber); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		worlds[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := updateStatement.Exec(worlds[i].RandomNumber, worlds[i].ID); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
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

	if !debugFlag {
		log.SetOutput(ioutil.Discard)
	}

	if err := startListening(listener); err != nil {
		log.Fatal(err)
	}
}
