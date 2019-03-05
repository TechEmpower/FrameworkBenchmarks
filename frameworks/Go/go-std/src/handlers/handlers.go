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

func queriesParam(r *http.Request) int {
	q, err := strconv.Atoi(r.URL.Query().Get("queries"))
	if err != nil || q < 1 {
		q = 1
	} else if q > 500 {
		q = 500
	}
	return q
}

// JSONHandler . Test 1: JSON serialization
func JSONHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "application/json")
	m := MessagePool.Get().(*Message)
	m.Message = "Hello, World!"
	json.NewEncoder(w).Encode(m)
	MessagePool.Put(m)
}

// DBHandler . Test 2: Single database query
func DBHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		world := storage.WorldPool.Get().(*storage.World)

		if err := db.GetOneRandomWorldPool(world); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(world)
		storage.WorldPool.Put(world)
	}
}

// QueriesHandler . Test 3: Multiple database queries
func QueriesHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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

// FortuneHandler . Test 4: Fortunes
func FortuneHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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

// FortuneQuickHandler . Test 4: Fortunes
func FortuneQuickHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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

// UpdateHandler . Test 5: Database updates
func UpdateHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte("Hello, World!"))
}
