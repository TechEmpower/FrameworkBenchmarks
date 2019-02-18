package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"html/template"
	"log"
	"math/rand"
	"net"
	"net/http"
	"os"
	"os/exec"
	"runtime"
	"sort"
	"strconv"
	"templates"

	_ "github.com/lib/pq"
)

type Message struct {
	Message string `json:"message"`
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
	// Content
	fortuneHTML = `<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
{{range .}}
<tr>
<td>{{.Id}}</td>
<td>{{.Message}}</td>
</tr>
{{end}}
</table>
</body>
</html>`

	// Database
	connectionString = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?sslmode=disable"
	worldSelect      = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdate      = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelect    = "SELECT id, message FROM Fortune"
	worldRowCount    = 10000
	maxConnections   = 256
)

var (
	// Templates
	tmpl = template.Must(template.New("fortune.html").Parse(fortuneHTML))

	// Database
	db                    *sql.DB
	worldSelectPrepared   *sql.Stmt
	worldUpdatePrepared   *sql.Stmt
	fortuneSelectPrepared *sql.Stmt
)

var prefork = flag.Bool("prefork", false, "use prefork")
var child = flag.Bool("child", false, "is child proc")

func initDB() {
	var err error
	db, err = sql.Open("postgres", connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnections)
	db.SetMaxOpenConns(maxConnections)

	worldSelectPrepared, err = db.Prepare(worldSelect)
	if err != nil {
		log.Fatal(err)
	}
	worldUpdatePrepared, err = db.Prepare(worldUpdate)
	if err != nil {
		log.Fatal(err)
	}
	fortuneSelectPrepared, err = db.Prepare(fortuneSelect)
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	var listener net.Listener
	flag.Parse()
	if !*prefork {
		runtime.GOMAXPROCS(runtime.NumCPU())
	} else {
		listener = doPrefork()
	}

	initDB()

	http.HandleFunc("/db", dbHandler)
	http.HandleFunc("/queries", queriesHandler)
	http.HandleFunc("/fortune", fortuneHandler)
	http.HandleFunc("/fortune-quick", fortuneQuickHandler)
	http.HandleFunc("/update", updateHandler)

	if !*prefork {
		http.ListenAndServe(":8080", nil)
	} else {
		http.Serve(listener, nil)
	}
}

func doPrefork() net.Listener {
	var listener net.Listener
	if !*child {
		addr, err := net.ResolveTCPAddr("tcp", ":8080")
		if err != nil {
			log.Fatal(err)
		}
		tcplistener, err := net.ListenTCP("tcp", addr)
		if err != nil {
			log.Fatal(err)
		}
		fl, err := tcplistener.File()
		if err != nil {
			log.Fatal(err)
		}
		children := make([]*exec.Cmd, runtime.NumCPU()/2)
		for i := range children {
			children[i] = exec.Command(os.Args[0], append(os.Args[1:], "-child")...)
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			children[i].ExtraFiles = []*os.File{fl}
			err = children[i].Start()
			if err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			if err := ch.Wait(); err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
	} else {
		var err error
		listener, err = net.FileListener(os.NewFile(3, ""))
		if err != nil {
			log.Fatal(err)
		}
	}
	return listener
}

func getQueriesParam(r *http.Request) int {
	n := 1
	if nStr := r.URL.Query().Get("queries"); len(nStr) > 0 {
		n, _ = strconv.Atoi(nStr)
	}

	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

// Test 2: Single database query
func dbHandler(w http.ResponseWriter, r *http.Request) {
	var world World
	err := worldSelectPrepared.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		log.Fatalf("Error scanning world row: %s", err.Error())
	}

	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(&world)
}

// Test 3: Multiple database queries
func queriesHandler(w http.ResponseWriter, r *http.Request) {
	n := getQueriesParam(r)

	world := make([]World, n)
	for i := 0; i < n; i++ {
		err := worldSelectPrepared.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
	}

	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(world)
}

// Test 4: Fortunes
func fortuneHandler(w http.ResponseWriter, r *http.Request) {
	rows, err := fortuneSelectPrepared.Query()
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}
	defer rows.Close()

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() {
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})
	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	if err := tmpl.Execute(w, fortunes); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Test 4: Fortunes
func fortuneQuickHandler(w http.ResponseWriter, r *http.Request) {
	rows, err := fortuneSelectPrepared.Query()
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}
	defer rows.Close()

	fortunes := make([]templates.Fortune, 0, 16)
	for rows.Next() { // Fetch rows
		fortune := templates.Fortune{}
		if err := rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err.Error())
		}
		fortunes = append(fortunes, fortune)
	}
	fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})

	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	templates.WriteFortunePage(w, fortunes)
}

// Test 5: Database updates
func updateHandler(w http.ResponseWriter, r *http.Request) {
	n := getQueriesParam(r)

	world := make([]World, n)
	for i := 0; i < n; i++ {
		if err := worldSelectPrepared.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %v", err)
		}
		world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
	}

	if len(world) > 0 {
		// against deadlocks
		sort.Slice(world, func(i, j int) bool {
			return world[i].Id < world[j].Id
		})

		tx, err := db.Begin()
		if err != nil {
			log.Fatal(err)
		}

		for i := 0; i < n; i++ {
			if _, err := tx.Stmt(worldUpdatePrepared).Exec(world[i].RandomNumber, world[i].Id); err != nil {
				log.Printf("Error updating world row: %v\n", err)
				tx.Rollback()
			}
		}

		if err := tx.Commit(); err != nil {
			log.Fatal(err)
		}
	}

	w.Header().Set("Server", "Go")
	w.Header().Set("Content-Type", "application/json")
	encoder := json.NewEncoder(w)
	encoder.Encode(world)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
