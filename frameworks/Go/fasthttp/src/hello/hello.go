package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"html/template"
	"log"
	"math/rand"
	"net"
	"os"
	"os/exec"
	"runtime"
	"sort"

	_ "github.com/go-sql-driver/mysql"
	"github.com/valyala/fasthttp"
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

// Databases
const (
	// Go 1.4's sql.DB has scalability problem when using (explicitly reused) prepared statement.
	// https://github.com/golang/go/issues/9484
	//
	// Using db.Query() instead of stmt.Query() avoid the issue.
	// But it makes 3 round trips per query: prepare, execute and close.
	// `interpolateParams=true` enables client side parameter interpolation.
	// It reduces round trips without prepared statement.
	//
	// Before Go 1.5 is released, we can see real power of Go with this benchmark.
	// After Go 1.5 is released, we can see prepared statement vs interpolation by comparing
	// this and another lightweight Go framework.
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world?interpolateParams=true"
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256
)

const helloWorldString = "Hello, World!"

var (
	// Templates
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	// Database
	db *sql.DB

	helloWorldBytes = []byte(helloWorldString)
)

var prefork = flag.Bool("prefork", false, "use prefork")
var child = flag.Bool("child", false, "is child proc")

func main() {
	var listener net.Listener
	flag.Parse()
	if !*prefork {
		runtime.GOMAXPROCS(runtime.NumCPU())
	} else {
		listener = doPrefork()
	}

	var err error
	db, err = sql.Open("mysql", connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnectionCount)

	s := &fasthttp.Server{
		Handler: func(ctx *fasthttp.RequestCtx) {
			path := ctx.Path()
			switch {
			case fasthttp.EqualBytesStr(path, "/plaintext"):
				plaintextHandler(ctx)
			case fasthttp.EqualBytesStr(path, "/json"):
				jsonHandler(ctx)
			case fasthttp.EqualBytesStr(path, "/db"):
				dbHandler(ctx)
			case fasthttp.EqualBytesStr(path, "/queries"):
				dbHandler(ctx)
			case fasthttp.EqualBytesStr(path, "/fortune"):
				fortuneHandler(ctx)
			default:
				ctx.Error("unexpected path", fasthttp.StatusBadRequest)
			}
		},
		Name: "fasthttp",
	}
	if !*prefork {
		s.ListenAndServe(":8080")
	} else {
		s.Serve(listener)
	}
}

func doPrefork() (listener net.Listener) {
	var err error
	var fl *os.File
	var tcplistener *net.TCPListener
	if !*child {
		var addr *net.TCPAddr
		addr, err = net.ResolveTCPAddr("tcp", ":8080")
		if err != nil {
			log.Fatal(err)
		}
		tcplistener, err = net.ListenTCP("tcp", addr)
		if err != nil {
			log.Fatal(err)
		}
		fl, err = tcplistener.File()
		if err != nil {
			log.Fatal(err)
		}
		children := make([]*exec.Cmd, runtime.NumCPU())
		for i := range children {
			children[i] = exec.Command(os.Args[0], "-prefork", "-child")
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			children[i].ExtraFiles = []*os.File{fl}
			err = children[i].Start()
			if err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			var err error = ch.Wait()
			if err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
	} else {
		fl = os.NewFile(3, "")
		listener, err = net.FileListener(fl)
		if err != nil {
			log.Fatal(err)
		}
		runtime.GOMAXPROCS(1)
	}
	return listener
}

func jsonMarshal(ctx *fasthttp.RequestCtx, v interface{}) {
	ctx.SetContentType("application/json")
	if err := json.NewEncoder(ctx).Encode(v); err != nil {
		log.Fatalf("error in json.Encoder.Encode: %s", err)
	}
}

// Test 1: JSON serialization
func jsonHandler(ctx *fasthttp.RequestCtx) {
	jsonMarshal(ctx, &Message{helloWorldString})
}

// Test 2: Single database query
func dbHandler(ctx *fasthttp.RequestCtx) {
	var world World
	err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
	if err != nil {
		log.Fatalf("Error scanning world row: %s", err)
	}

	jsonMarshal(ctx, &world)
}

// Test 3: Multiple database queries
func queriesHandler(ctx *fasthttp.RequestCtx) {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	world := make([]World, n)
	for i := 0; i < n; i++ {
		err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %s", err)
		}
	}

	jsonMarshal(ctx, world)
}

// Test 4: Fortunes
func fortuneHandler(ctx *fasthttp.RequestCtx) {
	rows, err := db.Query(fortuneSelect)
	if err != nil {
		log.Fatalf("Error preparing statement: %v", err)
	}

	fortunes := make(Fortunes, 0, 16)
	for rows.Next() { //Fetch rows
		fortune := Fortune{}
		if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err)
		}
		fortunes = append(fortunes, &fortune)
	}
	rows.Close()
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(ByMessage{fortunes})

	ctx.SetContentType("text/html")
	if err := tmpl.Execute(ctx, fortunes); err != nil {
		log.Fatalf("Error executing fortune: %s", err)
	}
}

// Test 5: Database updates
func updateHandler(ctx *fasthttp.RequestCtx) {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	world := make([]World, n)
	for i := 0; i < n; i++ {
		if err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
			log.Fatalf("Error scanning world row: %s", err)
		}
		world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
		if _, err := db.Exec(worldUpdate, world[i].RandomNumber, world[i].Id); err != nil {
			log.Fatalf("Error updating world row: %s", err)
		}
	}

	jsonMarshal(ctx, world)
}

// Test 6: Plaintext
func plaintextHandler(ctx *fasthttp.RequestCtx) {
	ctx.Success("text/plain", helloWorldBytes)
}

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
