package main

import (
	"encoding/json"
	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"
)

const (
	connectionString = "localhost"
	helloWorldString = "Hello, world!"
	worldRowCount    = 10000
)

var (
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	database *mgo.Database
	fortunes *mgo.Collection
	worlds   *mgo.Collection
)

type Message struct {
	Message string `json:"message"`
}

type World struct {
	Id           uint16 `bson:"id" json:"id"`
	RandomNumber uint16 `bson:"randomNumber" json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `bson:"id" json:"id"`
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
	runtime.GOMAXPROCS(runtime.NumCPU())
	if session, err := mgo.Dial(connectionString); err != nil {
		log.Fatalf("Error opening database: %v", err)
	} else {
		defer session.Close()
		session.SetPoolLimit(5)
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
}

// Helper for random numbers
func getRandomNumber() uint16 {
	return uint16(rand.Intn(worldRowCount) + 1)
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "go-mongodb")
	json.NewEncoder(w).Encode(&Message{helloWorldString})
}

func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	query := bson.M{"id": getRandomNumber()}
	if worlds != nil {
		if err := worlds.Find(query).One(&world); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
			return
		} else {
			w.Header().Set("Content-Type", "application/json")
			w.Header().Set("Server", "go-mongodb")
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

	if n > 500 {
		n = 500
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "go-mongodb")
	encoder := json.NewEncoder(w)

	if n <= 1 {
		result := make([]World, 1)
		query := bson.M{"id": getRandomNumber()}
		if err := worlds.Find(query).One(&result[0]); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
			return
		}
		encoder.Encode(&result)
	} else {
		result := make([]World, n)
		for i := 0; i < n; i++ {
			query := bson.M{"id": getRandomNumber()}
			if err := worlds.Find(query).One(&result[i]); err != nil {
				log.Fatalf("Error finding world with id: %s", err.Error())
				return
			}
		}
		encoder.Encode(&result)
	}
}

func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	w.Header().Set("Server", "go-mongodb")
	f := make(Fortunes, 16)
	if err := fortunes.Find(nil).All(&f); err == nil {
		f = append(f, Fortune{
			Message: "Additional fortune added at request time.",
		})
		sort.Sort(ByMessage{f})
		if err := tmpl.Execute(w, f); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
	}

}

func updateHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Server", "go-mongodb")
	encoder := json.NewEncoder(w)

	if n <= 1 {
		result := make([]World, 1)
		colQuery := bson.M{"id": getRandomNumber()}
		update := bson.M{"$set": bson.M{"randomNumber": getRandomNumber()}}
		if err := worlds.Update(colQuery, update); err != nil {
			log.Fatalf("Error updating world with id: %s", err.Error())
		} else {
			result[0].Id = colQuery["id"].(uint16)
			result[0].RandomNumber = update["$set"].(bson.M)["randomNumber"].(uint16)
		}
		encoder.Encode(&result)
	} else {
		if n > 500 {
			n = 500
		}
		result := make([]World, n)
		for i := 0; i < n; i++ {
			colQuery := bson.M{"id": getRandomNumber()}
			update := bson.M{"$set": bson.M{"randomNumber": getRandomNumber()}}
			if err := worlds.Update(colQuery, update); err != nil {
				log.Fatalf("Error updating world with id: %s", err.Error())
			} else {
				result[i].Id = colQuery["id"].(uint16)
				result[i].RandomNumber = update["$set"].(bson.M)["randomNumber"].(uint16)
			}
		}
		encoder.Encode(&result)
	}
}

func plaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Header().Set("Server", "go-mongodb")
	w.Write([]byte(helloWorldString))
}
