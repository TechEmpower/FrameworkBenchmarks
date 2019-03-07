package handlers

import (
	"encoding/json"
	"log"
	"net/http"
	"sort"
	"strconv"

	"github.com/valyala/bytebufferpool"

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

// // QueriesHandler . Test 3: Multiple database queries
// func QueriesHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
// 	return func(w http.ResponseWriter, r *http.Request) {
// 		queries := queriesParam(r)
// 		worlds := make([]storage.World, queries)

// 		var err error
// 		for i := 0; i < queries; i++ {
// 			worlds[i], err = db.GetOneRandomWorld()
// 			if err != nil {
// 				log.Println(err)
// 			}
// 		}

// 		w.Header().Set("Server", "Go")
// 		w.Header().Set("Content-Type", "application/json")
// 		json.NewEncoder(w).Encode(worlds)
// 	}
// }

// QueriesHandler . Test 3: Multiple database queries
func QueriesHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		queries := queriesParam(r)
		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorldPool(&worlds[i]); err != nil {
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

// // FortuneHandler . Test 4: Fortunes
// func FortuneHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
// 	return func(w http.ResponseWriter, r *http.Request) {
// 		fortunes, err := db.GetFortunes()
// 		if err != nil {
// 			http.Error(w, err.Error(), http.StatusInternalServerError)
// 			return
// 		}
// 		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

// 		sort.Slice(fortunes, func(i, j int) bool {
// 			return fortunes[i].Message < fortunes[j].Message
// 		})

// 		w.Header().Set("Server", "Go")
// 		w.Header().Set("Content-Type", "text/html; charset=utf-8")
// 		if err := templates.FortuneTemplate.Execute(w, fortunes); err != nil {
// 			http.Error(w, err.Error(), http.StatusInternalServerError)
// 		}
// 	}
// }

// // FortuneQuickHandler . Test 4: Fortunes
// func FortuneQuickHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
// 	return func(w http.ResponseWriter, r *http.Request) {
// 		fortunes, err := db.GetFortunes()
// 		if err != nil {
// 			http.Error(w, err.Error(), http.StatusInternalServerError)
// 			return
// 		}
// 		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

// 		sort.Slice(fortunes, func(i, j int) bool {
// 			return fortunes[i].Message < fortunes[j].Message
// 		})

// 		w.Header().Set("Server", "Go")
// 		w.Header().Set("Content-Type", "text/html; charset=utf-8")
// 		templates.WriteFortunePage(w, fortunes)
// 	}
// }

// FortuneQuickHandler . Test 4: Fortunes
func FortuneQuickHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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

// // UpdateHandler . Test 5: Database updates
// func UpdateHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
// 	return func(w http.ResponseWriter, r *http.Request) {
// 		queries := queriesParam(r)
// 		worlds := make([]storage.World, queries)
// 		var err error

// 		for i := 0; i < queries; i++ {
// 			if worlds[i], err = db.GetOneRandomWorld(); err != nil {
// 				log.Println(err)
// 			}
// 		}

// 		if err = db.UpdateWorlds(worlds, queries); err != nil {
// 			log.Println(err)
// 			return
// 		}

// 		w.Header().Set("Server", "Go")
// 		w.Header().Set("Content-Type", "application/json")
// 		json.NewEncoder(w).Encode(worlds)
// 	}
// }

// UpdateHandler . Test 5: Database updates
func UpdateHandler(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		queries := queriesParam(r)
		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]
		var err error

		// for _, world := range worlds {
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorldPool(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorldsPool(worlds, queries); err != nil {
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

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "text/plain")
	b := bytebufferpool.Get()
	b.SetString("Hello, World!")
	w.Write(b.Bytes())
	b.Reset()
	bytebufferpool.Put(b)
}
