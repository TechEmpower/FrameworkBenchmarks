package main

import (
	"database/sql"
	"github.com/fitstar/falcore"
	"html/template"
	"io"
	"log"
	"math/rand"
	"net/http"
	"runtime"
	"sort"
	"strconv"
	"sync"
	"time"

	_ "github.com/go-sql-driver/mysql"
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
	// Database
	connectionString   = "benchmarkdbuser:benchmarkdbpass@tcp(localhost:3306)/hello_world"
	worldSelect        = "SELECT id, randomNumber FROM World WHERE id = ?"
	worldUpdate        = "UPDATE World SET randomNumber = ? WHERE id = ?"
	fortuneSelect      = "SELECT id, message FROM Fortune;"
	worldRowCount      = 10000
	maxConnectionCount = 256

	helloWorldString = "Hello, World!"
)

var (
	// Templates
	tmpl = template.Must(template.ParseFiles("templates/layout.html", "templates/fortune.html"))

	// Database
	worldStatement   *sql.Stmt
	fortuneStatement *sql.Stmt
	updateStatement  *sql.Stmt

	helloWorldBytes = []byte(helloWorldString)
)

type stats struct {
	Count time.Duration
	Sum   time.Duration
}

// for profiling
var reqCount = 0
var statMap = make(map[string]*stats)
var compCallbackMutex = &sync.Mutex{}

func CompletionCallback(req *falcore.Request, res *http.Response) {
	l := req.PipelineStageStats
	compCallbackMutex.Lock()
	incrStat(statMap, "overhead", req.Overhead)
	incrStat(statMap, "tots", req.EndTime.Sub(req.StartTime))
	for e := l.Front(); e != nil; e = e.Next() {
		pss, _ := e.Value.(*falcore.PipelineStageStat)
		dur := pss.EndTime.Sub(pss.StartTime)
		incrStat(statMap, pss.Name, dur)
	}
	reqCount++
	if reqCount%10000 == 0 {
		for k, v := range statMap {
			log.Printf("%v: %v\n", k, v.Sum/v.Count)
		}
		log.Println("")
	}
	compCallbackMutex.Unlock()
}

func incrStat(statMap map[string]*stats, name string, dur time.Duration) {
	if s, ok := statMap[name]; ok {
		s.Count++
		s.Sum += dur
	} else {
		statMap[name] = &stats{1, dur}
	}
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())

	db, err := sql.Open("mysql", connectionString)
	if err != nil {
		log.Fatalf("Error opening database: %v", err)
	}
	db.SetMaxIdleConns(maxConnectionCount)
	worldStatement, err = db.Prepare(worldSelect)
	if err != nil {
		log.Fatal(err)
	}
	fortuneStatement, err = db.Prepare(fortuneSelect)
	if err != nil {
		log.Fatal(err)
	}
	updateStatement, err = db.Prepare(worldUpdate)
	if err != nil {
		log.Fatal(err)
	}
	pipeline := falcore.NewPipeline()

	pipeline.Upstream.PushBack(dbFilter)
	pipeline.Upstream.PushBack(queriesFilter)
	pipeline.Upstream.PushBack(jsonFilter)
	pipeline.Upstream.PushBack(fortuneFilter)
	pipeline.Upstream.PushBack(updateFilter)
	pipeline.Upstream.PushBack(plaintextFilter)

	pipeline.Downstream.PushBack(requiredHeaders)

	/*
		http.HandleFunc("/db", dbHandler)
		http.HandleFunc("/queries", queriesHandler)
		http.HandleFunc("/json", jsonHandler)
		http.HandleFunc("/fortune", fortuneHandler)
		http.HandleFunc("/update", updateHandler)
		http.HandleFunc("/plaintext", plaintextHandler)
	*/
	server := falcore.NewServer(8080, pipeline)
	// uncomment for printing internal peromance stats
	//server.CompletionCallback = CompletionCallback

	if err := server.ListenAndServe(); err != nil {
		log.Println("Could not start server:", err)
	}
	//if err := http.ListenAndServe(":8080", server); err != nil {
	//      log.Println("Could not start server:", err)
	//}
}

var requiredHeaders = falcore.NewResponseFilter(func(req *falcore.Request, res *http.Response) {
	res.Header.Set("Server", "falcore")
	res.Header.Set("Date", time.Now().Format(time.RFC1123))
})

var applicationJson = http.Header{"Content-Type": []string{"application/json"}}
var textPlain = http.Header{"Content-Type": []string{"text/plain"}}
var textHtml = http.Header{"Content-Type": []string{"text/html"}}

// Test 1: JSON serialization
var jsonFilter = falcore.NewRequestFilter(func(req *falcore.Request) *http.Response {
	if req.HttpRequest.URL.Path == "/json" {
		resp, _ := falcore.JSONResponse(req.HttpRequest, 200, applicationJson, &Message{helloWorldString})
		return resp
	}
	return nil
})

// Test 2: Single database query
var dbFilter = falcore.NewRequestFilter(func(req *falcore.Request) *http.Response {
	if req.HttpRequest.URL.Path == "/db" {
		var world World
		err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
		if err != nil {
			log.Fatalf("Error scanning world row: %s", err.Error())
		}

		resp, _ := falcore.JSONResponse(req.HttpRequest, 200, applicationJson, &world)
		return resp
	}
	return nil
})

// Test 3: Multiple database queries
var queriesFilter = falcore.NewRequestFilter(func(req *falcore.Request) *http.Response {
	if req.HttpRequest.URL.Path == "/queries" {
		n := 1
		if nStr := req.HttpRequest.URL.Query().Get("queries"); len(nStr) > 0 {
			n, _ = strconv.Atoi(nStr)
		}

		if n <= 1 {
			return dbFilter.FilterRequest(req)
		}

		world := make([]World, n)
		for i := 0; i < n; i++ {
			err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
			if err != nil {
				log.Fatalf("Error scanning world row: %s", err.Error())
			}
		}
		resp, _ := falcore.JSONResponse(req.HttpRequest, 200, applicationJson, &world)
		return resp
	}
	return nil
})

// Test 4: Fortunes
var fortuneFilter = falcore.NewRequestFilter(func(req *falcore.Request) *http.Response {
	if req.HttpRequest.URL.Path == "/fortune" {
		rows, err := fortuneStatement.Query()
		if err != nil {
			log.Fatalf("Error preparing statement: %v", err)
		}

		fortunes := make(Fortunes, 0, 16)
		for rows.Next() { //Fetch rows
			fortune := Fortune{}
			if err := rows.Scan(&fortune.Id, &fortune.Message); err != nil {
				log.Fatalf("Error scanning fortune row: %s", err.Error())
			}
			fortunes = append(fortunes, &fortune)
		}
		fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

		sort.Sort(ByMessage{fortunes})
		pipeReader, pipeWriter := io.Pipe()
		// TODO maybe figure out err handling
		go func() {
			tmpl.Execute(pipeWriter, fortunes)
			pipeWriter.Close()
		}()

		return falcore.SimpleResponse(req.HttpRequest, 200, textHtml, -1, pipeReader)
	}
	return nil
})

// Test 5: Database updates
var updateFilter = falcore.NewRequestFilter(func(req *falcore.Request) *http.Response {
	if req.HttpRequest.URL.Path == "/update" {
		n := 1
		if nStr := req.HttpRequest.URL.Query().Get("queries"); len(nStr) > 0 {
			n, _ = strconv.Atoi(nStr)
		}

		if n <= 1 {
			var world World
			worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
			world.RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
			updateStatement.Exec(world.RandomNumber, world.Id)
			resp, _ := falcore.JSONResponse(req.HttpRequest, 200, applicationJson, &world)
			return resp
		} else {
			world := make([]World, n)
			for i := 0; i < n; i++ {
				if err := worldStatement.QueryRow(rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
					log.Fatalf("Error scanning world row: %s", err.Error())
				}
				world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
				if _, err := updateStatement.Exec(world[i].RandomNumber, world[i].Id); err != nil {
					log.Fatalf("Error updating world row: %s", err.Error())
				}
			}
			resp, _ := falcore.JSONResponse(req.HttpRequest, 200, applicationJson, world)
			return resp
		}

	}
	return nil
})

// Test 6: Plaintext
var plaintextFilter = falcore.NewRequestFilter(func(req *falcore.Request) *http.Response {
	if req.HttpRequest.URL.Path == "/plaintext" {
		return falcore.ByteResponse(req.HttpRequest, 200, textPlain, helloWorldBytes)
	}
	return nil
})

type Fortunes []*Fortune

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }
