package main

import (
	"database/sql"
	"flag"
	"log"
	"runtime"

	_ "github.com/go-sql-driver/mysql"
	"github.com/valyala/fasthttp"

	"common"
	"templates"
)

const connectionString = "benchmarkdbuser:benchmarkdbpass@tcp(tfb-database:3306)/hello_world"

var (
	worldSelectStmt   *sql.Stmt
	worldUpdateStmt   *sql.Stmt
	fortuneSelectStmt *sql.Stmt

	db *sql.DB
)

func main() {
	flag.Parse()

	var err error
	if db, err = sql.Open("mysql", connectionString); err != nil {
		log.Fatalf("Error opening database: %s", err)
	}
	if err = db.Ping(); err != nil {
		log.Fatalf("Cannot connect to db: %s", err)
	}

	maxConnectionCount := runtime.NumCPU() * 2
	db.SetMaxIdleConns(maxConnectionCount)
	db.SetMaxOpenConns(maxConnectionCount)

	worldSelectStmt = mustPrepare(db, "SELECT id, randomNumber FROM World WHERE id = ?")
	worldUpdateStmt = mustPrepare(db, "UPDATE World SET randomNumber = ? WHERE id = ?")
	fortuneSelectStmt = mustPrepare(db, "SELECT id, message FROM Fortune")

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
	rows, err := fortuneSelectStmt.Query()
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
	stmt := txn.Stmt(worldUpdateStmt)
	for i := 0; i < n; i++ {
		w := &worlds[i]
		if _, err := stmt.Exec(w.RandomNumber, w.Id); err != nil {
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
	if err := worldSelectStmt.QueryRow(n).Scan(&w.Id, &w.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err)
	}
}

func mustPrepare(db *sql.DB, query string) *sql.Stmt {
	stmt, err := db.Prepare(query)
	if err != nil {
		log.Fatalf("Error when preparing statement %q: %s", query, err)
	}
	return stmt
}
