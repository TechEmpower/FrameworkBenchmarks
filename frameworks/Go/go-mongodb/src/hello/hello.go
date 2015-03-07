package main

import (
	"encoding/json"
	"fmt"
	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"strconv"
)

const (
	connectionString = "localhost"
	helloWorldString = "Hello, world!"
	worldRowCount    = 10000
)

var (
	collection *mgo.Collection
	database   *mgo.Database
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

func main() {
	port := ":8228"
	runtime.GOMAXPROCS(runtime.NumCPU())
	if session, err := mgo.Dial(connectionString); err != nil {
		log.Fatalf("Error opening database: %v", err)
	} else {
		defer session.Close()
		session.SetPoolLimit(5)
		database = session.DB("hello_world")
		collection = database.C("world")
		http.HandleFunc("/db", dbHandler)
		http.HandleFunc("/json", jsonHandler)
		fmt.Println("Serving on http://localhost" + port)
		http.ListenAndServe(port, nil)
	}
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}

func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	query := bson.M{
		"id": rand.Intn(worldRowCount) + 1,
	}
	if collection != nil {
		if err := collection.Find(query).One(&world); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
			return
		} else {
			w.Header().Set("Content-Type", "application/json")
			json.NewEncoder(w).Encode(&world)
			return
		}
	} else {
		log.Fatal("Collection not initialized properly")
	}
}

func queriesHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n <= 1 {
		dbHandler(w, r)
		return
	} else if n > 500 {
		n = 500
	}

	worlds := make([]World, n)
	for _, world := range worlds {
		query := bson.M{
			"id": rand.Intn(worldRowCount) + 1,
		}
		if err := collection.Find(query).One(&world); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
			return
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(worlds)
}
