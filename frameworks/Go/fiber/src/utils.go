package main

import (
	"context"
	"fmt"
	"math/rand"
	"runtime"
	"strconv"
	"sync"

	"github.com/gofiber/fiber"
	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/tidwall/sjson"
)

const (
	helloText      = "Hello, World!"
	jsonPath       = "message"
	worldCount     = 10000
	worldSelectSQL = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdateSQL = "UPDATE World SET randomNumber = $1 WHERE id = $2"
)

// https://github.com/tidwall/sjson
var (
	db       *pgxpool.Pool
	helloRaw = []byte(helloText)
	jsonRaw  = []byte(`{"message":""}`)
	jsonOpt  = &sjson.Options{
		Optimistic: true,
	}
)

// Worlds ...
type Worlds []World

// World ...
type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
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
		return make(Worlds, 0, 512)
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

// initDatabase :
func initDatabase() {
	maxConn := runtime.NumCPU()
	if maxConn == 0 {
		maxConn = 8
	}
	var err error
	db, err = pgxpool.Connect(context.Background(), fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s pool_max_conns=%d", "tfb-database", 5432, "benchmarkdbuser", "benchmarkdbpass", "hello_world", maxConn))
	if err != nil {
		panic(err)
	}
}

func RandomWorld() int {
	return rand.Intn(worldCount) + 1
}

// QueriesCount :
func QueriesCount(c *fiber.Ctx) int {
	n, _ := strconv.Atoi(c.Query("queries"))
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}
