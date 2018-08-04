package main

import (
	"flag"
	"fmt"
	"log"
	"runtime"

	"github.com/restream/reindexer"
	"github.com/valyala/fasthttp"

	"common"
	"templates"
)

var (
	db *reindexer.Reindexer
)

func main() {
	flag.Parse()

	var err error
	maxConnectionCount := runtime.NumCPU() * 4

	if db, err = initDatabase("cproto://tfb-database:6534/hello_world", maxConnectionCount); err != nil {
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
	it := db.Query("fortune").Exec()
	defer it.Close()

	if it.Error() != nil {
		log.Fatalf("Error selecting db data: %v", it.Error())
	}

	fortunes := make([]templates.Fortune, 0, 16)
	for it.Next() {
		obj := *it.Object().(*Fortune)
		fortunes = append(fortunes, templates.Fortune{ID: obj.ID, Message: obj.Message})
	}
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

	for i := 0; i < n; i++ {
		w := &worlds[i]
		if up, err := db.Update("world", w); err != nil || up == 0 {
			log.Fatalf("Error updating world row %d: %s", i, err)
		}
	}

	common.JSONMarshal(ctx, worlds)
}

func fetchRandomWorld(w *common.World) {
	n := common.RandomWorldNum()

	if item, err := db.Query("world").WhereInt("id", reindexer.EQ, n).Exec().FetchOne(); err != nil {
		log.Fatalf("Error scanning world row: %s", err)
	} else {
		*w = *item.(*common.World)
	}
}

type Fortune struct {
	ID      int32  `json:"id"`
	Message string `json:"message"`
}

func initDatabase(dbDsn string, maxConnectionsInPool int) (*reindexer.Reindexer, error) {

	db := reindexer.NewReindex(dbDsn, reindexer.WithConnPoolSize(maxConnectionsInPool))

	if err := db.Ping(); err != nil {
		return nil, err
	}

	if err := db.OpenNamespace("world", reindexer.DefaultNamespaceOptions(), &common.World{}); err != nil {
		return nil, err
	}
	if err := db.OpenNamespace("fortune", reindexer.DefaultNamespaceOptions(), &Fortune{}); err != nil {
		return nil, err
	}

	fmt.Println("--------------------------------------------------------------------------------------------")

	log.Println("Fetching one record to test if db connection is valid...")
	n := common.RandomWorldNum()

	if _, errPing := db.Query("world").WhereInt("id", reindexer.EQ, n).Exec().FetchOne(); errPing != nil {
		log.Fatalf("Error scanning world row: %s", errPing)
	}
	log.Println("OK")

	fmt.Println("--------------------------------------------------------------------------------------------")

	return db, nil
}
