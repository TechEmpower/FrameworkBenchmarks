package handlers

import (
	"encoding/json"
	"log"
	"net/http"
	"sort"

	"go-std/src/storage"
	"go-std/src/templates"

	"github.com/mailru/easyjson"
)

// Test 1: JSON serialization
func jsonHandlerEasyJSON(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	easyjson.MarshalToHTTPResponseWriter(&Message{"Hello, World!"}, w)
}

// Test 2: Single database query
func dbHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		world, err := db.GetOneRandomWorld()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		w.Header().Set("Server", "Go")
		easyjson.MarshalToHTTPResponseWriter(&world, w)
	}
}

// Test 3: Multiple database queries
func queriesHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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
		easyjson.MarshalToHTTPResponseWriter(&results, w)
	}
}

// Test 4: Fortunes
func fortuneHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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
func fortuneQuickHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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
func updateHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		q := queriesParam(r)

		worlds, err := db.UpdateRandomWorlds(q)
		if err != nil {
			log.Println(err)
			return
		}

		w.Header().Set("Server", "Go")
		w.Header().Set("Content-Type", "application/json")
		encoder := json.NewEncoder(w)
		encoder.Encode(worlds)
	}
}
