package main

import (
	"context"
	"fmt"
	"github.com/cloudwego/hertz/pkg/app"
	"github.com/cloudwego/hertz/pkg/app/server"
	"github.com/cloudwego/hertz/pkg/app/server/render"
	"github.com/cloudwego/hertz/pkg/common/config"
	"github.com/cloudwego/hertz/pkg/protocol"
	"github.com/cloudwego/hertz/pkg/protocol/consts"
	"github.com/goccy/go-json"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
	"math/rand"
	"runtime"
	"sort"
	"sync"
	"unsafe"
)

var (
	db *pgxpool.Pool
)

const (
	// Database
	helloworld    = "Hello, World!"
	worldSelect   = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdate   = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelect = "SELECT id, message FROM Fortune;"

	worldRowCount = 10000
	jsonpath      = "/json"
	dbpath        = "/db"
	dbspath       = "/dbs"
	fortunespath  = "/fortunes"
	updatepath    = "/update"
	plaintextpath = "/plaintext"
)

var helloworldRaw = []byte("Hello, World!")

type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

type Fortune struct {
	Id      uint16 `json:"id"`
	Message string `json:"message"`
}

type Fortunes []*Fortune
type Worlds []World

func (s Fortunes) Len() int      { return len(s) }
func (s Fortunes) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

type ByMessage struct{ Fortunes }

func (s ByMessage) Less(i, j int) bool { return s.Fortunes[i].Message < s.Fortunes[j].Message }

func parseQueries(c context.Context, ctx *app.RequestContext) int {
	n := GetUintOrZeroFromArgs(ctx.QueryArgs(), "queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

// / Test 1: JSON serialization
func jsonhandler(c context.Context, ctx *app.RequestContext) {
	m := AcquireJSON()
	m.Message = helloworld
	ctx.JSON(200, &m)
	ReleaseJSON(m)
}

// / Test 2: Single database query
func dbHandler(c context.Context, ctx *app.RequestContext) {
	world := AcquireWorld()
	db.QueryRow(context.Background(), worldSelect, RandomWorld()).Scan(&world.Id, &world.RandomNumber)
	ctx.JSON(200, &world)
	ReleaseWorld(world)
}

// / Test 3: Multiple database queries
func dbs(c context.Context, ctx *app.RequestContext) {
	n := parseQueries(c, ctx)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldSelect, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
	}
	ctx.JSON(200, &worlds)
	ReleaseWorlds(worlds)
}

// / Test 4: Fortunes
func fortunes(c context.Context, ctx *app.RequestContext) {
	rows, _ := db.Query(context.Background(), fortuneSelect)
	fortunes := make(Fortunes, 0)
	for rows.Next() { //Fetch rows
		fortune := Fortune{}
		_ = rows.Scan(&fortune.Id, &fortune.Message)
		fortunes = append(fortunes, &fortune)
	}
	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})
	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})
	ctx.HTML(200, "fortune.html", fortunes)
}

// / Test 5: Database updates
func update(c context.Context, ctx *app.RequestContext) {
	n := parseQueries(c, ctx)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		w := &worlds[i]
		db.QueryRow(context.Background(), worldSelect, RandomWorld()).Scan(&w.Id, &w.RandomNumber)
		w.RandomNumber = int32(RandomWorld())
	}

	// sorting is required for insert deadlock prevention.
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].Id < worlds[j].Id
	})
	batch := pgx.Batch{}
	for _, w := range worlds {
		batch.Queue(worldUpdate, w.RandomNumber, w.Id)
	}
	db.SendBatch(context.Background(), &batch).Close()
	ctx.JSON(200, &worlds)
	ReleaseWorlds(worlds)
}

// / Test 6: plaintext
func plaintext(c context.Context, ctx *app.RequestContext) {
	ctx.SetStatusCode(consts.StatusOK)
	ctx.Response.SetBodyString(helloworld)
}

func main() {
	h := server.New(config.Option{F: func(o *config.Options) {
		o.Addr = ":8080"
		o.DisableHeaderNamesNormalizing = true
	}})
	render.ResetJSONMarshal(json.Marshal)
	h.Use(func(c context.Context, ctx *app.RequestContext) {
		switch b2s(ctx.Path()) {
		case plaintextpath:
			plaintext(c, ctx)
		case jsonpath:
			jsonhandler(c, ctx)
		case dbpath:
			dbHandler(c, ctx)
		case dbspath:
			dbs(c, ctx)
		case updatepath:
			update(c, ctx)
		case fortunespath:
			fortunes(c, ctx)
		}
	})
	h.LoadHTMLGlob("/templates/fortune.html")
	h.Spin()
}

func init() {
	maxConn := runtime.NumCPU() * 4

	var err error
	db, err = pgxpool.New(context.Background(),
		fmt.Sprintf(
			"host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d",
			"tfb-database", 5432,
			"benchmarkdbuser",
			"benchmarkdbpass",
			"hello_world",
			maxConn,
		))
	if err != nil {
		panic(err)
	}
}

type Message struct {
	Message string `json:"message"`
}

// JSONpool ...
var JSONpool = sync.Pool{
	New: func() interface{} {
		return new(Message)
	},
}

// AcquireJSON ...
func AcquireJSON() *Message {
	return JSONpool.Get().(*Message)
}

// ReleaseJSON ...
func ReleaseJSON(json *Message) {
	json.Message = ""
	JSONpool.Put(json)
}

// WorldPool ...
var WorldPool = sync.Pool{
	New: func() interface{} {
		return new(World)
	},
}

// AcquireWorld ...
func AcquireWorld() *World {
	return WorldPool.Get().(*World)
}

// ReleaseWorld ...
func ReleaseWorld(w *World) {
	w.Id = 0
	w.RandomNumber = 0
	WorldPool.Put(w)
}

// WorldsPool ...
var WorldsPool = sync.Pool{
	New: func() interface{} {
		return make(Worlds, 0, 500)
	},
}

// AcquireWorlds ...
func AcquireWorlds() Worlds {
	return WorldsPool.Get().(Worlds)
}

// ReleaseWorlds ...ReleaseWorlds
func ReleaseWorlds(w Worlds) {
	w = w[:0]
	WorldsPool.Put(w)
}

func b2s(b []byte) string {
	return unsafe.String(unsafe.SliceData(b), len(b))
}

// RandomWorld :
func RandomWorld() int {
	return rand.Intn(worldRowCount) + 1
}

func GetUintOrZeroFromArgs(a *protocol.Args, key string) int {
	b := a.Peek(key)
	n := len(b)
	if n == 0 {
		return 0
	}
	v := 0
	for i := 0; i < n; i++ {
		c := b[i]
		k := c - '0'
		if k > 9 {
			return 0
		}
		vNew := 10*v + int(k)
		if vNew < v {
			return 0
		}
		v = vNew
	}
	return v
}
