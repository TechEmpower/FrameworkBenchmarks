package main

import (
	"encoding/json"
	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"sort"
	"strconv"
)

const (
	connectionString = "localhost"
	helloWorldString = "Hello, world!"
	worldRowCount    = 10000
)

var (
	tmpl            = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))
	helloWorldBytes = []byte(helloWorldString)

	database *mgo.Database
	fortunes *mgo.Collection
	worlds   *mgo.Collection
)

type Message struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `bson:"_id" json:"id"`
	RandomNumber uint16 `bson:"randomNumber" json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `bson:"_id" json:"id"`
	Message string `bson:"message" json:"message"`
}

type Fortunes []Fortune

func (s Fortunes) Len() int {
	return len(s)
}

func (s Fortunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool {
	return s.Fortunes[i].Message < s.Fortunes[j].Message
}

func main() {
	session, err := mgo.Dial(connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	defer session.Close()
	database = session.DB("hello_world")
	worlds = database.C("world")
	fortunes = database.C("fortune")
	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/fortune", fortuneHandler)
	http.HandleFunc("/queries", queriesHandler)
	http.HandleFunc("/update", updateHandler)
	http.HandleFunc("/plaintext", plaintextHandler)
	http.ListenAndServe(":8080", nil)
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "go-mongodb")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}

// Helper for random numbers
func getRandomNumber() uint16 {
	return uint16(rand.Intn(worldRowCount) + 1)
}

// Test 2: Single database query
func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	query := bson.M{"_id": getRandomNumber()}
	if err := worlds.Find(query).One(&world); err != nil {
		log.Fatalf("Error finding world with id: %s", err.Error())
	}
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "go-mongodb")
	json.NewEncoder(w).Encode(&world)
}

// Helper for getting the "queries" parameter
func getQueriesParam(r *http.Request) int {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	return n
}

// Test 3: Multiple database queries
func queriesHandler(w http.ResponseWriter, r *http.Request) {
	n := getQueriesParam(r)
	world := make([]World, n)

	for i := 0; i < n; i++ {
		query := bson.M{"_id": getRandomNumber()}
		if err := worlds.Find(query).One(&world[i]); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "go-mongodb")
	json.NewEncoder(w).Encode(world)
}

// Test 4: Fortunes
func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	w.Header().Set("Server", "go-mongodb")
	f := make(Fortunes, 16)
	if err := fortunes.Find(nil).All(&f); err == nil {
		f = append(f, Fortune{Message: "Additional fortune added at request time."})
		sort.Sort(ByMessage{f})
		if err := tmpl.Execute(w, f); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
	} else {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 5: Database updates
func updateHandler(w http.ResponseWriter, r *http.Request) {
	n := getQueriesParam(r)
	world := make([]World, n)

	for i := 0; i < n; i++ {
		query := bson.M{"_id": getRandomNumber()}
		if err := worlds.Find(query).One(&world[i]); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
		}
		world[i].RandomNumber = getRandomNumber()
		update := bson.M{"$set": bson.M{"randomNumber": world[i].RandomNumber}}
		if err := worlds.Update(query, update); err != nil {
			log.Fatalf("Error updating world with id: %s", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "Go")
	json.NewEncoder(w).Encode(&world)
}

// Test 6: Plaintext
func plaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Header().Set("Server", "go-mongodb")
	w.Write(helloWorldBytes)
}
