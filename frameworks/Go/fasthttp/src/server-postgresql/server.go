package main

import (
	"flag"
	"fmt"
	"log"
	"runtime"

	"github.com/jackc/pgx"
	"github.com/valyala/fasthttp"

	"common"
	"templates"
)

var (
	worldSelectStmt   *pgx.PreparedStatement
	worldUpdateStmt   *pgx.PreparedStatement
	fortuneSelectStmt *pgx.PreparedStatement

	db *pgx.ConnPool
)

func main() {
	flag.Parse()

	var err error
	maxConnectionCount := runtime.NumCPU() * 4
	if db, err = initDatabase("tfb-database", "benchmarkdbuser", "benchmarkdbpass", "hello_world", 5432, maxConnectionCount); err != nil {
		log.Fatalf("Error opening database: %s", err)
	}

	s := &fasthttp.Server{
		Handler: mainHandler,
		Name:    "go",
	}
	ln := common.GetListener()
	if err = s.Serve(ln); err != nil {
		log.Fatalf("Error when serving incoming connections: %s", err)
	}
}

func mainHandler(ctx *fasthttp.RequestCtx) {
	path := ctx.Path()
	switch string(path) {
	case "/plaintext":
		common.PlaintextHandler(ctx)
	case "/json":
		common.JSONHandler(ctx)
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

func dbHandler(ctx *fasthttp.RequestCtx) {
	var w common.World
	fetchRandomWorld(&w)
	common.JSONMarshal(ctx, &w)
}

func queriesHandler(ctx *fasthttp.RequestCtx) {
	n := common.GetQueriesCount(ctx)
	worlds := make([]common.World, n)
	for i := 0; i < n; i++ {
		fetchRandomWorld(&worlds[i])
	}
	common.JSONMarshal(ctx, worlds)
}

func fortuneHandler(ctx *fasthttp.RequestCtx) {
	rows, err := db.Query("fortuneSelectStmt")
	if err != nil {
		log.Fatalf("Error selecting db data: %v", err)
	}

	var f templates.Fortune
	fortunes := make([]templates.Fortune, 0, 16)
	for rows.Next() {
		if err := rows.Scan(&f.ID, &f.Message); err != nil {
			log.Fatalf("Error scanning fortune row: %s", err)
		}
		fortunes = append(fortunes, f)
	}
	rows.Close()
	fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

	common.SortFortunesByMessage(fortunes)

	ctx.SetContentType("text/html; charset=utf-8")
	templates.WriteFortunePage(ctx, fortunes)
}

func updateHandler(ctx *fasthttp.RequestCtx) {
	n := common.GetQueriesCount(ctx)

	worlds := make([]common.World, n)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		fetchRandomWorld(w)
		w.RandomNumber = int32(common.RandomWorldNum())
	}

	// sorting is required for insert deadlock prevention.
	common.SortWorldsByID(worlds)

	txn, err := db.Begin()
	if err != nil {
		log.Fatalf("Error starting transaction: %s", err)
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

	common.JSONMarshal(ctx, worlds)
}

func fetchRandomWorld(w *common.World) {
	n := common.RandomWorldNum()
	if err := db.QueryRow("worldSelectStmt", n).Scan(&w.Id, &w.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err)
	}
}

func mustPrepare(db *pgx.Conn, name, query string) *pgx.PreparedStatement {
	stmt, err := db.Prepare(name, query)
	if err != nil {
		log.Fatalf("Error when preparing statement %q: %s", query, err)
	}
	return stmt
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

	config.AfterConnect = func(conn *pgx.Conn) error {
		worldSelectStmt = mustPrepare(conn, "worldSelectStmt", "SELECT id, randomNumber FROM World WHERE id = $1")
		worldUpdateStmt = mustPrepare(conn, "worldUpdateStmt", "UPDATE World SET randomNumber = $1 WHERE id = $2")
		fortuneSelectStmt = mustPrepare(conn, "fortuneSelectStmt", "SELECT id, message FROM Fortune")
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
		var w common.World
		n := common.RandomWorldNum()
		if errPing := connPool.QueryRow("worldSelectStmt", n).Scan(&w.Id, &w.RandomNumber); errPing != nil {
			log.Fatalf("Error scanning world row: %s", errPing)
		}
		log.Println("OK")
	}

	fmt.Println("--------------------------------------------------------------------------------------------")

	return connPool, err
}
