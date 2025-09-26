package handler

import (
	"context"
	"net/http"
	"sort"

	"github.com/bytedance/sonic"
	"github.com/jackc/pgx/v4"

	"goframe/template"
)

const (
	helloWorldStr    = "Hello, World!"
	contentTypePlain = "text/plain; charset=utf-8"
	contentTypeHtml  = "text/html; charset=utf-8"
	contentTypeJson  = "application/json"
)

var (
	ctx             = context.Background()
	helloWorldBytes = []byte(helloWorldStr)
	worldsCache     = &Worlds{W: make([]World, worldsCount)}
)

// PopulateWorldsCache populates the worlds cache for the cache test.
func PopulateWorldsCache() {
	rows, err := db.Query(ctx, worldSelectCacheSQL, len(worldsCache.W))
	if err != nil {
		panic(err)
	}
	i := 0
	for rows.Next() {
		w := &worldsCache.W[i]
		if err = rows.Scan(&w.ID, &w.RandomNumber); err != nil {
			panic(err)
		}
		i++
	}
}

// JSON . Test 1: JSON serialization.
func JSON(w http.ResponseWriter, r *http.Request) {
	message := acquireMessage()
	message.Message = helloWorldStr
	output, _ := sonic.Marshal(message)
	w.Header().Set("Content-Type", contentTypeJson)
	_, _ = w.Write(output)
	releaseMessage(message)
}

// DB . Test 2: Single database query.
func DB(w http.ResponseWriter, r *http.Request) {
	world := acquireWorld()
	_ = db.QueryRow(ctx, worldSelectSQL, randomWorldNum()).Scan(&world.ID, &world.RandomNumber)
	output, _ := sonic.Marshal(world)
	w.Header().Set("Content-Type", contentTypeJson)
	_, _ = w.Write(output)
	releaseWorld(world)
}

// Queries . Test 3: Multiple database queries.
func Queries(w http.ResponseWriter, r *http.Request) {
	var (
		queries = queriesParam(r)
		worlds  = acquireWorlds()
	)
	worlds.W = worlds.W[:queries]
	for i := 0; i < queries; i++ {
		world := &worlds.W[i]
		_ = db.QueryRow(ctx, worldSelectSQL, randomWorldNum()).Scan(&world.ID, &world.RandomNumber)
	}
	output, _ := sonic.Marshal(worlds.W)
	w.Header().Set("Content-Type", contentTypeJson)
	_, _ = w.Write(output)
	releaseWorlds(worlds)
}

// CachedWorlds . Test 4: Multiple cache queries.
func CachedWorlds(w http.ResponseWriter, r *http.Request) {
	var (
		queries = queriesParam(r)
		worlds  = acquireWorlds()
	)
	worlds.W = worlds.W[:queries]
	for i := 0; i < queries; i++ {
		worlds.W[i] = worldsCache.W[randomWorldNum()-1]
	}
	output, _ := sonic.Marshal(worlds.W)
	w.Header().Set("Content-Type", contentTypeJson)
	_, _ = w.Write(output)
	releaseWorlds(worlds)
}

// FortunesQuick . Test 5: Fortunes.
func FortunesQuick(w http.ResponseWriter, r *http.Request) {
	var (
		fortune  = template.AcquireFortune()
		fortunes = template.AcquireFortunes()
	)
	rows, _ := db.Query(ctx, fortuneSelectSQL)
	for rows.Next() {
		_ = rows.Scan(&fortune.ID, &fortune.Message) // nolint:errcheck
		fortunes.F = append(fortunes.F, *fortune)
	}

	fortune.ID = 0
	fortune.Message = "Additional fortune added at request time."
	fortunes.F = append(fortunes.F, *fortune)

	sort.Slice(fortunes.F, func(i, j int) bool {
		return fortunes.F[i].Message < fortunes.F[j].Message
	})

	w.Header().Set("Content-Type", contentTypeHtml)
	template.WriteFortunePage(w, fortunes.F)

	template.ReleaseFortune(fortune)
	template.ReleaseFortunes(fortunes)
}

// Updates . Test 6: Database updates.
func Updates(w http.ResponseWriter, r *http.Request) {
	var (
		queries = queriesParam(r)
		worlds  = acquireWorlds()
	)
	worlds.W = worlds.W[:queries]
	for i := 0; i < queries; i++ {
		world := &worlds.W[i]
		_ = db.QueryRow(ctx, worldSelectSQL, randomWorldNum()).Scan(&world.ID, &world.RandomNumber)
		world.RandomNumber = int32(randomWorldNum())
	}
	// against deadlocks
	sort.Slice(worlds.W, func(i, j int) bool {
		return worlds.W[i].ID < worlds.W[j].ID
	})
	batch := new(pgx.Batch)
	for i := 0; i < queries; i++ {
		world := &worlds.W[i]
		batch.Queue(worldUpdateSQL, world.RandomNumber, world.ID)
	}
	_ = db.SendBatch(ctx, batch).Close()

	output, _ := sonic.Marshal(worlds.W)
	w.Header().Set("Content-Type", contentTypeJson)
	_, _ = w.Write(output)

	releaseWorlds(worlds)
}

// Plaintext . Test 7: Plaintext.
func Plaintext(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", contentTypePlain)
	_, _ = w.Write(helloWorldBytes)
}
