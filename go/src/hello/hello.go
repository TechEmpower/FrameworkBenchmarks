package main

import (
	"database/sql"
	"encoding/json"
	_ "github.com/go-sql-driver/mysql"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"strconv"
)

type MessageStruct struct {
	Message string
}

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

const (
	DB_CONN_STR   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world?charset=utf8"
	DB_SELECT_SQL = "SELECT * FROM World where id = ?;"
	DB_ROWS       = 10000
)

var (
	db    *sql.DB
	query *sql.Stmt
)

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	var err error
	if db, err = sql.Open("mysql", DB_CONN_STR); err != nil {
		log.Fatalf("Error opening database: %s", err)
	}
	if query, err = db.Prepare(DB_SELECT_SQL); err != nil {
		log.Fatalf("Error preparing statement: %s", err)
	}
	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/db", dbHandler)
	http.ListenAndServe(":8080", nil)
}

func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(&MessageStruct{"Hello, world"})
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func dbHandler(w http.ResponseWriter, r *http.Request) {
	qnum := 1
	if qnumStr := r.URL.Query().Get("queries"); len(qnumStr) != 0 {
		qnum, _ = strconv.Atoi(qnumStr)
	}
	ww := make([]World, qnum)
	for i := 0; i < qnum; i++ {
		query.QueryRow(rand.Intn(DB_ROWS)+1).Scan(&ww[i].Id, &ww[i].RandomNumber)
	}
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(ww)
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}
