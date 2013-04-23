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
	DB_CONN_STR   = "benchmarkdbuser:benchmarkdbpass@tcp(172.16.98.98:3306)/hello_world?charset=utf8"
	DB_SELECT_SQL = "SELECT id, randomNumber FROM World where id = ?"
	DB_ROWS       = 10000
	MAX_CON       = 80
)

var (
	stmts = make(chan *sql.Stmt, MAX_CON)
)

func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(&MessageStruct{"Hello, world"})
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func dbHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if qnumStr := r.URL.Query().Get("queries"); len(qnumStr) != 0 {
		n, _ = strconv.Atoi(qnumStr)
	}
	stmt := <-stmts // wait for a connection
	ww := make([]World, n)
	for i := 0; i < n; i++ {
		stmt.QueryRow(rand.Intn(DB_ROWS)+1).Scan(
			&ww[i].Id,
			&ww[i].RandomNumber,
		)
	}
	stmts <- stmt // return a connection
	j, _ := json.Marshal(ww)
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func main() {
	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/json", jsonHandler)
	http.ListenAndServe(":8080", nil)
}

func init() {
	// use cores
	runtime.GOMAXPROCS(runtime.NumCPU())
	// setup connection pool
	for i := 0; i < MAX_CON; i++ {
		if db, err := sql.Open("mysql", DB_CONN_STR); err == nil {
			stmt, err := db.Prepare(DB_SELECT_SQL)
			if err != nil {
				log.Fatal(err)
			}
			stmts <- stmt
		} else {
			log.Fatalf("Error opening database: %s", err)
		}
	}
}
