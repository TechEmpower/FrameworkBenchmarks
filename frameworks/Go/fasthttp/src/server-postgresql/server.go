package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"unsafe"

	pgx "github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/valyala/fasthttp"

	"fasthttp/src/common"
	"fasthttp/src/templates"
)

var (
	db *pgxpool.Pool
)

const worldSelectSQL = "SELECT id, randomNumber FROM World WHERE id = $1"
const worldUpdateSQL = "UPDATE World SET randomNumber = $1 WHERE id = $2"
const fortuneSelectSQL = "SELECT id, message FROM Fortune"

func main() {
	bindHost := flag.String("bind", ":8080", "set bind host")
	prefork := flag.Bool("prefork", false, "use prefork")
	child := flag.Bool("child", false, "is child proc")
	flag.Parse()

	var err error
	maxConnectionCount := common.NumCPU() * 4
	if *child {
		maxConnectionCount = common.NumCPU()
	}
	if db, err = initDatabase("tfb-database", "benchmarkdbuser", "benchmarkdbpass", "hello_world", 5432, maxConnectionCount); err != nil {
		log.Fatalf("Error opening database: %s", err)
	}

	s := &fasthttp.Server{
		Handler: mainHandler,
		Name:    "go",
	}

	var ln net.Listener
	if *prefork {
		ln = common.DoPrefork(*child, *bindHost)
	} else {
		ln = common.GetListener(*bindHost)
	}

	if err = s.Serve(ln); err != nil {
		log.Fatalf("Error when serving incoming connections: %s", err)
	}
}

func mainHandler(ctx *fasthttp.RequestCtx) {
	path := ctx.Path()
	switch *(*string)(unsafe.Pointer(&path)) {
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
	w := fetchRandomWorld()
	wb, err := w.MarshalJSON()
	if err != nil {
		log.Println(err)
		return
	}
	ctx.SetContentType("application/json")
	ctx.Write(wb)
}

func queriesHandler(ctx *fasthttp.RequestCtx) {
	n := common.GetQueriesCount(ctx)
	worlds := make([]common.World, n)
	for i := 0; i < n; i++ {
		worlds[i] = fetchRandomWorld()
	}
	wb, err := common.Worlds(worlds).MarshalJSON()
	if err != nil {
		log.Println(err)
		return
	}
	ctx.SetContentType("application/json")
	ctx.Write(wb)
}

func fortuneHandler(ctx *fasthttp.RequestCtx) {
	rows, err := db.Query(context.Background(), fortuneSelectSQL)
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
		worlds[i] = fetchRandomWorld()
		worlds[i].RandomNumber = int32(common.RandomWorldNum())
	}

	// sorting is required for insert deadlock prevention.
	common.SortWorldsByID(worlds)

	batch := pgx.Batch{}
	for _, w := range worlds {
		batch.Queue(worldUpdateSQL, w.RandomNumber, w.Id)
	}
	if err := db.SendBatch(context.Background(), &batch).Close(); err != nil {
		log.Fatalf("Error when closing a batch: %s", err)
	}

	wb, err := common.Worlds(worlds).MarshalJSON()
	if err != nil {
		log.Println(err)
		return
	}
	ctx.SetContentType("application/json")
	ctx.Write(wb)
}

func fetchRandomWorld() (w common.World) {
	n := common.RandomWorldNum()
	if err := db.QueryRow(context.Background(), worldSelectSQL, n).Scan(&w.Id, &w.RandomNumber); err != nil {
		log.Fatalf("Error scanning world row: %s", err)
	}
	return w
}

func initDatabase(dbHost string, dbUser string, dbPass string, dbName string, dbPort uint16, maxConnectionsInPool int) (*pgxpool.Pool, error) {
	var successOrFailure string = "OK"

	dsn := fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d", dbHost, dbPort, dbUser, dbPass, dbName, maxConnectionsInPool)

	fmt.Println("--------------------------------------------------------------------------------------------")

	connPool, err := pgxpool.Connect(context.Background(), dsn)
	if err != nil {
		successOrFailure = "FAILED"
		log.Println("Connecting to database ", dbName, " as user ", dbUser, " ", successOrFailure, ": \n ", err)
	} else {
		log.Println("Connecting to database ", dbName, " as user ", dbUser, ": ", successOrFailure)

		log.Println("Fetching one record to test if db connection is valid...")
		var w common.World
		n := common.RandomWorldNum()
		if errPing := connPool.QueryRow(context.Background(), worldSelectSQL, n).Scan(&w.Id, &w.RandomNumber); errPing != nil {
			log.Fatalf("Error scanning world row: %s", errPing)
		}
		log.Println("OK")
	}

	fmt.Println("--------------------------------------------------------------------------------------------")

	return connPool, err
}
