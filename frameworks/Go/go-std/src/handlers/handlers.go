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
	m := MessagePool.Get().(*Message)
	m.Message = "Hello, World!"
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(m)
	MessagePool.Put(m)
}

// DBHandler . Test 2: Single database query
func DBHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		world := storage.WorldPool.Get().(*storage.World)

		if err := db.GetOneRandomWorld(world); err != nil {
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
		queries := queriesParam(r)

		// worlds := make([]storage.World, queries)
		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(worlds)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)
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

// FortuneHandlerPool . Test 4: Fortunes
func FortuneHandlerPool(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		fortunes, err := db.GetFortunesPool()
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
		if err = templates.FortuneTemplate.Execute(w, fortunes); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
		fortunes = fortunes[:0]
		templates.FortunesPool.Put(fortunes)
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

// FortuneQuickHandlerPool . Test 4: Fortunes
func FortuneQuickHandlerPool(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		fortunes, err := db.GetFortunesPool()
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

		fortunes = fortunes[:0]
		templates.FortunesPool.Put(fortunes)
	}
}

// UpdateHandler . Test 5: Database updates
func UpdateHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		queries := queriesParam(r)
		var err error

		// worlds := make([]storage.World, queries)
		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		// for _, world := range worlds {
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorlds(worlds); err != nil {
			log.Println(err)
			return
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(worlds)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)
	}
}

var helloWorld = []byte("Hello, World!")

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "text/plain")
	// As per 6, iv:
	//  This test is not intended to exercise the allocation of memory or
	//  instantiation of objects. Therefore it is acceptable but not required
	//  to re-use a single buffer for the response text.
	w.Write(helloWorld)
}
