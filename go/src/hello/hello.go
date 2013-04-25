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
	"sync"
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
	ConnectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world"
	WorldSelect        = "SELECT id, randomNumber FROM World where id = ?"
	FortuneSelect      = "SELECT id, message FROM Fortune;"
	WorldRowCount      = 10000
	MaxConnectionCount = 100
)

var (
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	worldStatement    *sql.Stmt
	fourtuneStatement *sql.Stmt
)

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())

	db, err := sql.Open("mysql", ConnectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(MaxConnectionCount)
	worldStatement, err = db.Prepare(WorldSelect)
	if err != nil {
		log.Fatal(err)
	}
	fourtuneStatement, err = db.Prepare(FortuneSelect)
	if err != nil {
		log.Fatal(err)
	}

	http.HandleFunc("/db", worldHandler)
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

func worldHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) != 0 {
		n, _ = strconv.Atoi(nStr)
	}
	ww := make([]World, n)
	if n == 1 {
		worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).Scan(&ww[0].Id, &ww[0].RandomNumber)
	} else {
		var wg sync.WaitGroup
		wg.Add(n)
		for i := 0; i < n; i++ {
			go func(i int) {
				err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).Scan(&ww[i].Id, &ww[i].RandomNumber)
				if err != nil {
					log.Fatalf("Error scanning world row: %v", err)
				}
				wg.Done()
			}(i)
		}
		wg.Wait()
	}
	j, _ := json.Marshal(ww)
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Content-Length", strconv.Itoa(len(j)))
	w.Write(j)
}

func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	fortunes := make([]*Fortune, 0, 16)

	//Execute the query
	rows, err := fourtuneStatement.Query()
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}

	i := 0
	var fortune *Fortune
	for rows.Next() { //Fetch rows
		fortune = new(Fortune)
		if err = rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %v", err)
		}
		fortunes = append(fortunes, fortune)
		i++
	}
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
