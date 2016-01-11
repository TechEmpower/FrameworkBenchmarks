package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"html/template"
	"log"
	"math/rand"
	"net"
	"os"
	"os/exec"
	"runtime"
	"sort"
	"sync/atomic"
	"time"

	"github.com/jackc/pgx"
	"github.com/valyala/fasthttp"
	"github.com/valyala/fasthttp/reuseport"
)

type Message struct {
	Message string `json:"message"`
}

type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

type Fortune struct {
	Id      int32  `json:"id"`
	Message string `json:"message"`
}

const (
	worldRowCount      = 10000
	maxConnectionCount = 256
)

var (
	worldSelectStmt   *pgx.PreparedStatement
	worldUpdateStmt   *pgx.PreparedStatement
	fortuneSelectStmt *pgx.PreparedStatement
)

const helloWorldString = "Hello, World!"

var (
	tmpl = template.Must(template.ParseFiles("templates/fortune.html"))

	db *pgx.ConnPool

	helloWorldBytes = []byte(helloWorldString)
)

var (
	listenAddr = flag.String("listenAddr", ":8080", "Address to listen to")
	prefork    = flag.Bool("prefork", false, "use prefork")
	child      = flag.Bool("child", false, "is child proc")
)

func main() {
	flag.Parse()

	var err error

	// initialize the connection pool
	dbConns := maxConnectionCount
	if *prefork {
		dbConns = (maxConnectionCount + runtime.NumCPU() - 1) / runtime.NumCPU()
	}
	if db, err = initDatabase("localhost", "benchmarkdbuser", "benchmarkdbpass", "hello_world", 5432, dbConns); err != nil {
		log.Fatalf("Error opening database: %s", err)
	}

	s := &fasthttp.Server{
		Handler: mainHandler,
		Name:    "fasthttp",
	}
	ln := getListener()
	if err = s.Serve(ln); err != nil {
		log.Fatalf("Error when serving incoming connections: %s", err)
	}
}

const maxConnDuration = time.Millisecond * 200

var connDurationJitter uint64

func mainHandler(ctx *fasthttp.RequestCtx) {
	// Performance hack for prefork mode - periodically close keepalive
	// connections for evenly distributing connections among available
	// processes.
	if *prefork {
		maxDuration := maxConnDuration + time.Millisecond*time.Duration(atomic.LoadUint64(&connDurationJitter))
		if time.Since(ctx.ConnTime()) > maxDuration {
			atomic.StoreUint64(&connDurationJitter, uint64(rand.Intn(100)))
			ctx.SetConnectionClose()
		}
	}

	path := ctx.Path()
	switch string(path) {
	case "/plaintext":
		plaintextHandler(ctx)
	case "/json":
		jsonHandler(ctx)
	case "/db":
		dbHandler(ctx)
	case "/queries":
		queriesHandler(ctx)
	case "/fortune":
		fortuneHandler(ctx)
	case "/update":
		updateHandler(ctx)
	default:
		ctx.Error("unexpected path", fasthttp.StatusBadRequest)
	}
}

// Test 1: JSON serialization
func jsonHandler(ctx *fasthttp.RequestCtx) {
	jsonMarshal(ctx, &Message{helloWorldString})
}

// Test 2: Single database query
func dbHandler(ctx *fasthttp.RequestCtx) {
	var w World
	fetchRandomWorld(&w)
	jsonMarshal(ctx, &w)
}

// Test 3: Multiple database queries
func queriesHandler(ctx *fasthttp.RequestCtx) {
	n := getQueriesCount(ctx)

	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		fetchRandomWorld(&worlds[i])
	}

	jsonMarshal(ctx, worlds)
}

// Test 4: Fortunes
func fortuneHandler(ctx *fasthttp.RequestCtx) {
	rows, err := db.Query("fortuneSelectStmt")
	if err != nil {
		log.Fatalf("Error selecting db data: %v", err)
	}

	fortunes := make([]Fortune, 0, 16)
	for rows.Next() {
		var f Fortune
		if err := rows.Scan(&f.Id, &f.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err)
		}
		fortunes = append(fortunes, f)
	}
	rows.Close()
	fortunes = append(fortunes, Fortune{Message: "Additional fortune added at request time."})

	sort.Sort(FortunesByMessage(fortunes))

	ctx.SetContentType("text/html; charset=utf-8")
	if err := tmpl.Execute(ctx, fortunes); err != nil {
		log.Fatalf("Error executing fortune: %s", err)
	}
}

// Test 5: Database updates
func updateHandler(ctx *fasthttp.RequestCtx) {
	n := getQueriesCount(ctx)

	worlds := make([]World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		fetchRandomWorld(w)
		w.RandomNumber = int32(randomWorldNum())
	}

	// sorting is required for insert deadlock prevention.
	sort.Sort(WorldsByID(worlds))
	txn, err := db.Begin()
	if err != nil {
		log.Fatalf("Error starting transaction: %s", err)
	}

	// Disable synchronous commit for the current transaction
	// as a performance optimization.
	// See http://www.postgresql.org/docs/current/static/runtime-config-wal.html for details.
	// Below is the relevant quote from the docs:
	//
	// > It is therefore possible, and useful, to have some transactions
	// > commit synchronously and others asynchronously. For example,
	// > to make a single multistatement transaction commit asynchronously
	// > when the default is the opposite, issue
	// >     SET LOCAL synchronous_commit TO OFF
	// > within the transaction.
	if _, err = txn.Exec("SET LOCAL synchronous_commit TO OFF"); err != nil {
		log.Fatalf("Error when disabling synchronous commit")
	}

	for i := 0; i < n; i++ {
		w := &worlds[i]
		if _, err = txn.Exec("worldUpdateStmt", w.RandomNumber, w.Id); err != nil {
			log.Fatalf("Error updating world row %d: %s", i, err)
		}
	}
	if err = txn.Commit(); err != nil {
		log.Fatalf("Error when commiting world rows: %s", err)
	}

	jsonMarshal(ctx, worlds)
}

// Test 6: Plaintext
func plaintextHandler(ctx *fasthttp.RequestCtx) {
	ctx.Write(helloWorldBytes)
}

func jsonMarshal(ctx *fasthttp.RequestCtx, v interface{}) {
	ctx.SetContentType("application/json")
	if err := json.NewEncoder(ctx).Encode(v); err != nil {
		log.Fatalf("error in json.Encoder.Encode: %s", err)
	}
}

func fetchRandomWorld(w *World) {
	n := randomWorldNum()

	if err := db.QueryRow("worldSelectStmt", n).Scan(&w.Id, &w.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err)
	}
}

func randomWorldNum() int {
	return rand.Intn(worldRowCount) + 1
}

func getQueriesCount(ctx *fasthttp.RequestCtx) int {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

type FortunesByMessage []Fortune

func (s FortunesByMessage) Len() int           { return len(s) }
func (s FortunesByMessage) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s FortunesByMessage) Less(i, j int) bool { return s[i].Message < s[j].Message }

type WorldsByID []World

func (w WorldsByID) Len() int           { return len(w) }
func (w WorldsByID) Swap(i, j int)      { w[i], w[j] = w[j], w[i] }
func (w WorldsByID) Less(i, j int) bool { return w[i].Id < w[j].Id }

func mustPrepare(db *pgx.Conn, name, query string) *pgx.PreparedStatement {
	stmt, err := db.Prepare(name, query)
	if err != nil {
		log.Fatalf("Error when preparing statement %q: %s", query, err)
	}
	return stmt
}

func getListener() net.Listener {
	if !*prefork {
		runtime.GOMAXPROCS(runtime.NumCPU())
		ln, err := net.Listen("tcp4", *listenAddr)
		if err != nil {
			log.Fatal(err)
		}
		return ln
	}

	if !*child {
		children := make([]*exec.Cmd, runtime.NumCPU())
		for i := range children {
			children[i] = exec.Command(os.Args[0], "-prefork", "-child")
			children[i].Stdout = os.Stdout
			children[i].Stderr = os.Stderr
			if err := children[i].Start(); err != nil {
				log.Fatal(err)
			}
		}
		for _, ch := range children {
			if err := ch.Wait(); err != nil {
				log.Print(err)
			}
		}
		os.Exit(0)
		panic("unreachable")
	}

	runtime.GOMAXPROCS(1)
	ln, err := reuseport.Listen("tcp4", *listenAddr)
	if err != nil {
		log.Fatal(err)
	}
	return ln
}

func initDatabase(dbHost string, dbUser string, dbPass string, dbName string, dbPort uint16, maxConnectionsInPool int) (*pgx.ConnPool, error) {

	var successOrFailure string = "OK"

	var config pgx.ConnPoolConfig

	config.Host = dbHost
	config.User = dbUser
	config.Password = dbPass
	config.Database = dbName
	config.Port = dbPort

	config.MaxConnections = maxConnectionsInPool

	config.AfterConnect = func(eachConn *pgx.Conn) error {

		worldSelectStmt = mustPrepare(eachConn, "worldSelectStmt", "SELECT id, randomNumber FROM World WHERE id = $1")
		worldUpdateStmt = mustPrepare(eachConn, "worldUpdateStmt", "UPDATE World SET randomNumber = $1 WHERE id = $2")
		fortuneSelectStmt = mustPrepare(eachConn, "fortuneSelectStmt", "SELECT id, message FROM Fortune")

		return nil
	}

	fmt.Println("--------------------------------------------------------------------------------------------")

	connPool, err := pgx.NewConnPool(config)
	if err != nil {
		successOrFailure = "FAILED"
		log.Println("Connecting to database ", dbName, " as user ", dbUser, " ", successOrFailure, ": \n ", err)
	} else {
		log.Println("Connecting to database ", dbName, " as user ", dbUser, ": ", successOrFailure)

		log.Println("Fetching one record to test if db connection is valid...")
		var w World
		n := randomWorldNum()
		if errPing := connPool.QueryRow("worldSelectStmt", n).Scan(&w.Id, &w.RandomNumber); errPing != nil {
			log.Fatalf("Error scanning world row: %s", errPing)
		}
		log.Println("OK")
	}

	fmt.Println("--------------------------------------------------------------------------------------------")

	return connPool, err

}
