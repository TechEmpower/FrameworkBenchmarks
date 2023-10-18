package main

import (
	"context"
	"github.com/cloudwego/hertz/pkg/app"
	"github.com/cloudwego/hertz/pkg/app/server"
	"github.com/cloudwego/hertz/pkg/app/server/render"
	"github.com/cloudwego/hertz/pkg/common/config"
	"github.com/cloudwego/hertz/pkg/protocol"
	"github.com/goccy/go-json"
	"gorm.io/driver/postgres"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
	"math/rand"
	"sync"
	"unsafe"
)

const (
	helloworld = "Hello, World!"
)

var (
	db *gorm.DB
)

// World represents an entry int the World table
type World struct {
	ID           int64 `json:"id"`
	RandomNumber int64 `json:"randomNumber" gorm:"column:randomnumber"`
}

// TableName Override GORM convention for table mapping
func (World) TableName() string {
	return "World"
}

// implements the basic logic behind the query tests
func getWorld(db *gorm.DB) *World {
	// we could actually precompute a list of random
	// numbers and slice them but this makes no sense
	// as I expect that this 'random' is just a placeholder
	// for an actual business logic
	randomId := rand.Intn(10000) + 1

	var world World
	db.Take(&world, randomId)

	return &world
}

// implements the logic behind the updates tests
func processWorld(tx *gorm.DB) *World {
	// we could actually precompute a list of random
	// numbers and slice them but this makes no sense
	// as I expect that this 'random' is just a placeholder
	// for an actual business logic in a real test
	randomId := rand.Intn(10000) + 1
	randomId2 := int64(rand.Intn(10000) + 1)

	var world World
	tx.Take(&world, randomId)

	world.RandomNumber = randomId2
	_ = tx.Save(&world)

	return &world
}

func jsonhandler(c context.Context, ctx *app.RequestContext) {
	m := AcquireJSON()
	m.Message = helloworld
	ctx.JSON(200, &m)
	ReleaseJSON(m)
}

func plaintext(c context.Context, ctx *app.RequestContext) {
	ctx.SetStatusCode(200)
	ctx.SetBodyString("Hello, World!")
}

func dbHandler(c context.Context, ctx *app.RequestContext) {
	world := AcquireWorld()
	world = getWorld(db)
	ctx.JSON(200, &world)
	ReleaseWorld(world)
}

func parseQueries(c context.Context, ctx *app.RequestContext) int {
	n := getUintOrZeroFromArgs(ctx.QueryArgs(), "queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

func queries(c context.Context, ctx *app.RequestContext) {
	n := parseQueries(c, ctx)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		world := getWorld(db)
		worlds[i] = *world
	}
	ctx.JSON(200, &worlds)
	ReleaseWorlds(worlds)
}

func updates(c context.Context, ctx *app.RequestContext) {
	n := parseQueries(c, ctx)
	worlds := AcquireWorlds()[:n]
	for i := 0; i < n; i++ {
		world := processWorld(db)
		worlds[i] = *world
	}

	ctx.JSON(200, &worlds)
	ReleaseWorlds(worlds)
}
func main() {
	/* SETUP DB AND WEB SERVER */
	dsn := "host=tfb-database user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world port=5432 sslmode=disable"
	db, _ = gorm.Open(postgres.Open(dsn), &gorm.Config{
		PrepareStmt: true,                                  // use prep statements
		Logger:      logger.Default.LogMode(logger.Silent), // new, not inserted in original submission 2x on query
	})

	sqlDB, _ := db.DB()

	sqlDB.SetMaxIdleConns(500)

	h := server.New(config.Option{F: func(o *config.Options) {
		o.Addr = ":8080"
		o.DisableHeaderNamesNormalizing = true
	}})
	render.ResetJSONMarshal(json.Marshal)
	h.Use(func(c context.Context, ctx *app.RequestContext) {
		switch b2S(ctx.Path()) {
		case "/json":
			jsonhandler(c, ctx)
		case "/plaintext":
			plaintext(c, ctx)
		case "/db":
			dbHandler(c, ctx)
		case "/queries":
			queries(c, ctx)
		case "/updates":
			updates(c, ctx)
		}
	})
	h.Spin()
}

func b2S(b []byte) string {
	return *(*string)(unsafe.Pointer(&b))
}

type Message struct {
	Message string `json:"message"`
}

type Worlds []World

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
	w.ID = 0
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

func getUintOrZeroFromArgs(a *protocol.Args, key string) int {
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
