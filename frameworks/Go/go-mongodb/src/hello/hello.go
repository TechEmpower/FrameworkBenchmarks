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
	runtime.GOMAXPROCS(runtime.NumCPU())
	session, err := mgo.Dial(connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	fmt.Println("Test")
	defer session.Close()
	session.SetPoolLimit(5)
	http.HandleFunc("/json", jsonHandler)
	http.ListenAndServe(":8228", nil)
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}
