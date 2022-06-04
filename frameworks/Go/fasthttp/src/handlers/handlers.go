package handlers

import (
	"context"
	"encoding/json"
	"fasthttp/src/templates"
	"sort"

	pgx "github.com/jackc/pgx/v4"
	"github.com/valyala/fasthttp"
)

const (
	helloWorldStr = "Hello, World!"

	contentTypeJSON = "application/json"
	contentTypeHTML = "text/html; charset=utf-8"
)

var worldsCache = &Worlds{W: make([]World, worldsCount)}

// PopulateWorldsCache populates the worlds cache for the cache test.
func PopulateWorldsCache() {
	rows, err := db.Query(context.Background(), worldSelectCacheSQL, len(worldsCache.W))
	if err != nil {
		panic(err)
	}

	i := 0

	for rows.Next() {
		w := &worldsCache.W[i]

		if err := rows.Scan(&w.ID, &w.RandomNumber); err != nil {
			panic(err)
		}

		i++
	}
}

// JSON . Test 1: JSON serialization.
func JSON(ctx *fasthttp.RequestCtx) {
	message := acquireMessage()
	message.Message = helloWorldStr
	data, _ := json.Marshal(message)

	ctx.Response.Header.SetContentType(contentTypeJSON)
	ctx.Response.SetBody(data)

	releaseMessage(message)
}

// DB . Test 2: Single database query.
func DB(ctx *fasthttp.RequestCtx) {
	w := acquireWorld()
	id := randomWorldNum()

	db.QueryRow(context.Background(), worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
	data, _ := json.Marshal(w)

	ctx.Response.Header.SetContentType(contentTypeJSON)
	ctx.Response.SetBody(data)

	releaseWorld(w)
}

// Queries . Test 3: Multiple database queries.
func Queries(ctx *fasthttp.RequestCtx) {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := 0; i < queries; i++ {
		w := &worlds.W[i]
		id := randomWorldNum()

		db.QueryRow(context.Background(), worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
	}

	data, _ := json.Marshal(worlds.W)

	ctx.Response.Header.SetContentType(contentTypeJSON)
	ctx.Response.SetBody(data)

	releaseWorlds(worlds)
}

// CachedWorlds . Test 4: Multiple cache queries.
func CachedWorlds(ctx *fasthttp.RequestCtx) {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := 0; i < queries; i++ {
		worlds.W[i] = worldsCache.W[randomWorldNum()-1]
	}

	data, _ := json.Marshal(worlds.W)

	ctx.Response.Header.SetContentType(contentTypeJSON)
	ctx.Response.SetBody(data)

	releaseWorlds(worlds)
}

// FortunesQuick . Test 5: Fortunes.
func FortunesQuick(ctx *fasthttp.RequestCtx) {
	fortune := templates.AcquireFortune()
	fortunes := templates.AcquireFortunes()

	rows, _ := db.Query(context.Background(), fortuneSelectSQL)
	for rows.Next() {
		rows.Scan(&fortune.ID, &fortune.Message) // nolint:errcheck
		fortunes.F = append(fortunes.F, *fortune)
	}

	fortune.ID = 0
	fortune.Message = "Additional fortune added at request time."
	fortunes.F = append(fortunes.F, *fortune)

	sort.Slice(fortunes.F, func(i, j int) bool {
		return fortunes.F[i].Message < fortunes.F[j].Message
	})

	ctx.Response.Header.SetContentType(contentTypeHTML)
	templates.WriteFortunePage(ctx, fortunes.F)

	templates.ReleaseFortune(fortune)
	templates.ReleaseFortunes(fortunes)
}

// Updates . Test 6: Database updates.
func Updates(ctx *fasthttp.RequestCtx) {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := 0; i < queries; i++ {
		w := &worlds.W[i]
		id := randomWorldNum()

		db.QueryRow(context.Background(), worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
		w.RandomNumber = int32(randomWorldNum())
	}

	// against deadlocks
	sort.Slice(worlds.W, func(i, j int) bool {
		return worlds.W[i].ID < worlds.W[j].ID
	})

	batch := new(pgx.Batch)

	for i := 0; i < queries; i++ {
		w := &worlds.W[i]
		batch.Queue(worldUpdateSQL, w.RandomNumber, w.ID)
	}

	db.SendBatch(context.Background(), batch).Close()

	data, _ := json.Marshal(worlds.W)

	ctx.Response.Header.SetContentType(contentTypeJSON)
	ctx.Response.SetBody(data)

	releaseWorlds(worlds)
}

// Plaintext . Test 7: Plaintext.
func Plaintext(ctx *fasthttp.RequestCtx) {
	ctx.Response.SetBodyString(helloWorldStr)
}
