package handlers

import (
	"log"
	"net/http"

	"go-std/src/storage"

	"github.com/mailru/easyjson"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Server", "Go")
	easyjson.MarshalToHTTPResponseWriter(&Message{"Hello, World!"}, w)
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
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

// QueriesHandlerEasyJSON . Test 3: Multiple database queries
func QueriesHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		q := queriesParam(r)
		worlds := make([]storage.World, q)

		var err error
		for i := 0; i < q; i++ {
			worlds[i], err = db.GetOneRandomWorld()
			if err != nil {
				log.Println(err)
			}
		}

		w.Header().Set("Server", "Go")
		easyjson.MarshalToHTTPResponseWriter(storage.Worlds(worlds), w)
	}
}

// UpdateHandlerEasyJSON . Test 5: Database updates
func UpdateHandlerEasyJSON(db storage.DB) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		q := queriesParam(r)

		worlds, err := db.UpdateRandomWorlds(q)
		if err != nil {
			log.Println(err)
			return
		}

		w.Header().Set("Server", "Go")
		easyjson.MarshalToHTTPResponseWriter(storage.Worlds(worlds), w)
	}
}
