package handlers

import (
	"encoding/json"
	"log"
	"net/http"
	"sort"
	"strconv"

	"go-std/src/storage"
	"go-std/src/templates"
)

// var (
// 	// Database
// 	worldSelectPrepared   *sql.Stmt
// 	worldUpdatePrepared   *sql.Stmt
// 	fortuneSelectPrepared *sql.Stmt
// 	maxConnections        = runtime.NumCPU()
// )

func queriesParam(r *http.Request) int {
	q, err := strconv.Atoi(r.URL.Query().Get("queries"))
	if err != nil || q < 1 {
		q = 1
	} else if q > 500 {
		q = 500
	}
	return q
}

// Test 1: JSON serialization
func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&Message{"Hello, World!"})
}

// Test 2: Single database query
func dbHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		world, err := db.GetOneRandomWorld()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(&world)
	}
}

// Test 3: Multiple database queries
func queriesHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		q := queriesParam(r)
		results := make([]storage.World, q)

		var err error
		for i := 0; i < q; i++ {
			results[i], err = db.GetOneRandomWorld()
			if err != nil {
				log.Println(err)
			}
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(results)
	}
}

// Test 4: Fortunes
func fortuneHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		fortunes, err := db.GetFortunes()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		if err := templates.FortuneTemplate.Execute(w, fortunes); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
	}
}

// Test 4: Fortunes
func fortuneQuickHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		fortunes, err := db.GetFortunes()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		templates.WriteFortunePage(w, fortunes)
	}
}

// Test 5: Database updates
func updateHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		q := queriesParam(r)

		worlds, err := db.UpdateRandomWorlds(q)
		if err != nil {
			log.Println(err)
			return
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(worlds)
	}
}

// Test 6: Plaintext
func plaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte("Hello, World!"))
}
