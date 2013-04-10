package main

import (
	"encoding/json"
	"net/http"
	"runtime"
	"strconv"

	"database/sql"
	_ "github.com/go-sql-driver/mysql"
	"log"
	"math/rand"
)

const DB_ROWS = 10000

var db *sql.DB
var q *sql.Stmt

type MessageStruct struct {
	Message string
}
type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

func hello(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(&MessageStruct{"Hello, world"})
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func dbh(w http.ResponseWriter, r *http.Request) {
	qnum := 1
	qnumString := r.URL.Query().Get("queries")
	if len(qnumString) != 0 {
		qnum, _ = strconv.Atoi(qnumString)
	}
	ww := make([]World, qnum)
	for i := 0; i < qnum; i++ {
		q.QueryRow(rand.Intn(DB_ROWS)+1).Scan(&ww[i].Id, &ww[i].RandomNumber)
	}
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(ww)
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func main() {
	var err error
	runtime.GOMAXPROCS(runtime.NumCPU())

	db, err = sql.Open("mysql", "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world?charset=utf8")
	if err != nil {
		log.Fatalf("Error opening database: %s", err)
	}
	q, err = db.Prepare("SELECT * FROM World where id = ?;")
	if err != nil {
		log.Fatalf("Error preparing statement: %s", err)
	}

	http.HandleFunc("/json", hello)
	http.HandleFunc("/db", dbh)
	http.ListenAndServe(":8080", nil)
}
