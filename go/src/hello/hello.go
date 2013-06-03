package main

import (
	"database/sql"
	"encoding/json"
	"html/template"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"
	"sync"

	_ "github.com/go-sql-driver/mysql"
)

type Message struct {
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
	ConnectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world?charset=utf8"
	WorldSelect        = "SELECT id, randomNumber FROM World where id = ?"
	WorldUpdate        = "UPDATE World SET randomNumber = ? where id = ?"
	FortuneSelect      = "SELECT id, message FROM Fortune;"
	WorldRowCount      = 10000
	MaxConnectionCount = 5000
)

var (
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt
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
	fortuneStatement, err = db.Prepare(FortuneSelect)
	if err != nil {
		log.Fatal(err)
	}
	updateStatement, err = db.Prepare(WorldUpdate)
	if err != nil {
		log.Fatal(err)
	}

	http.HandleFunc("/db", worldHandler)
	http.HandleFunc("/json", jsonHandler)
	http.HandleFunc("/fortune", fortuneHandler)
	http.HandleFunc("/update", updateHandler)
	http.ListenAndServe(":8080", nil)
}

func jsonHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/javascript")
	json.NewEncoder(w).Encode(&Message{"Hello, world"})
}

func worldHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) != 0 {
		n, _ = strconv.Atoi(nStr)
	}
	ww := make([]World, n)
	if n == 1 {
		err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).Scan(&ww[0].Id, &ww[0].RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
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
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(ww)
}

func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	rows, err := fortuneStatement.Query()
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}

	fortunes := make([]*Fortune, 0, 16)
	for rows.Next() { //Fetch rows
		fortune := new(Fortune)
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %v", err)
		}
		fortunes = append(fortunes, fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	w.Header().Set("Content-Type", "text/html")
	if err := tmpl.Execute(w, map[string]interface{}{"fortunes": fortunes}); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func updateHandler(w http.ResponseWriter, r *http.Request) {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) != 0 {
		n, _ = strconv.Atoi(nStr)
	}
	ww := make([]World, n)
	if n == 1 {
		worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).Scan(&ww[0].Id, &ww[0].RandomNumber)
		ww[0].RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
		updateStatement.Exec(ww[0].RandomNumber, ww[0].Id)
	} else {
		var wg sync.WaitGroup
		wg.Add(n)
		for i := 0; i < n; i++ {
			go func(i int) {
				err := worldStatement.QueryRow(rand.Intn(WorldRowCount)+1).Scan(&ww[i].Id, &ww[i].RandomNumber)
				ww[i].RandomNumber = uint16(rand.Intn(WorldRowCount) + 1)
				updateStatement.Exec(ww[i].RandomNumber, ww[i].Id)
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

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
