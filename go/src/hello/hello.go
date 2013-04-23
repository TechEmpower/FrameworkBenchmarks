package main

import (
	"database/sql"
	"encoding/json"
	_ "github.com/go-sql-driver/mysql"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"
)

type MessageStruct struct {
	Message string
}

type World struct {
	Id           uint16 `json:"id"`
	RandomNumber uint16 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

const (
	DB_CONN_STR           = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world?charset=utf8"
	DB_SELECT_SQL         = "SELECT id, randomNumber FROM World where id = ?"
	DB_FORTUNE_SELECT_SQL = "SELECT id, message FROM Fortune;"
	DB_ROWS               = 10000
	MAX_CONN              = 80
)

var (
	stmts        = make(chan *sql.Stmt, MAX_CONN)
	fortuneStmts = make(chan *sql.Stmt, MAX_CONN)
	tmpl         = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))
)

func init() {
	//setup DB connection pool to work around Go issue #4805: https://code.google.com/p/go/issues/detail?id=4805&q=sql.db&colspec=ID%20Status%20Stars%20Priority%20Owner%20Reporter%20Summary
	for i := 0; i < MAX_CONN; i++ {
		db, err := sql.Open("mysql", DB_CONN_STR)
		if err != nil {
			log.Fatalf("Error opening database: %s", err)
		}
		stmt, err := db.Prepare(DB_SELECT_SQL)
		if err != nil {
			log.Fatal(err)
		}
		stmts <- stmt
		fortuneStmt, err := db.Prepare(DB_FORTUNE_SELECT_SQL)
		if err != nil {
			log.Fatal(err)
		}
		fortuneStmts <- fortuneStmt
	}
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/fortune", fortuneHandler)
	http.ListenAndServe(":8080", nil)
}

func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	j, _ := json.Marshal(&MessageStruct{"Hello, world"})
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func dbHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) != 0 {
		n, _ = strconv.Atoi(nStr)
	}
	ww := make([]World, n)
	stmt := <-stmts //wait for a connection
	for i := 0; i < n; i++ {
		stmt.QueryRow(rand.Intn(DB_ROWS)+1).Scan(&ww[i].Id, &ww[i].RandomNumber)
	}
	stmts <- stmt //get a connection
	j, _ := json.Marshal(ww)
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	//the Fortune table contains 12 rows, and we'll add another Fortune ourselves
	fortunes := make([]*Fortune, 0, 16)
	stmt := <-fortuneStmts    //wait for a connection
	rows, err := stmt.Query() //Execute the query
	if err != nil {
		log.Fatalf("Error preparing statement: %s", err)
	}
	i := 0
	var fortune *Fortune
	for rows.Next() { //Fetch rows
		fortune = new(Fortune)
		if err = rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			panic(err)
		}
		fortunes = append(fortunes, fortune)
		i++
	}
	fortuneStmts <- stmt //return a connection
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Sort(ByMessage{fortunes})
	w.Header().Set("Content-Type", "text/html")
	if err := tmpl.Execute(w, map[string]interface{}{"fortunes": fortunes}); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
