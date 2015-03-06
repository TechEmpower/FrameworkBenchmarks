package main

import (
	"encoding/json"
	"fmt"
	"gopkg.in/mgo.v2"
	"log"
	"net/http"
	"runtime"
)

const (
	connectionString = "localhost"
	helloWorldString = "Hello, world!"
	worldRowCount    = 10000
)

var (
	collection *mgo.Collection
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
	session, err := mgo.Dial(connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	defer session.Close()
	session.SetPoolLimit(5)
	collection = session.DB("hello_world").C("world")
	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/json", jsonHandler)
	fmt.Println("Serving on http://localhost" + port)
	http.ListenAndServe(port, nil)
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}

func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	var randomNumber = rand.Intn(worldRowCount) + 1
	query := bson.M{
		"id": randomNumber,
	}
	if collection != nil {
		if err := collection.Find(query).One(&World); err != nil {
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
