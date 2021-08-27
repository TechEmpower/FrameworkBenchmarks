package views

import (
	"atreugo/src/templates"
	"context"
	"sort"

	pgx "github.com/jackc/pgx/v4"
	"github.com/savsgio/atreugo/v11"
)

const (
	helloWorldStr = "Hello, World!"

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
func JSON(ctx *atreugo.RequestCtx) error {
	message := acquireMessage()
	message.Message = helloWorldStr
	err := ctx.JSONResponse(message)

	releaseMessage(message)

	return err
}

// DB . Test 2: Single database query.
func DB(ctx *atreugo.RequestCtx) error {
	w := acquireWorld()

	db.QueryRow(context.Background(), worldSelectSQL, randomWorldNum()).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
	err := ctx.JSONResponse(w)

	releaseWorld(w)

	return err
}

// Queries . Test 3: Multiple database queries.
func Queries(ctx *atreugo.RequestCtx) error {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := 0; i < queries; i++ {
		w := &worlds.W[i]
		db.QueryRow(context.Background(), worldSelectSQL, randomWorldNum()).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
	}

	err := ctx.JSONResponse(worlds.W)

	releaseWorlds(worlds)

	return err
}

// CachedWorlds . Test 4: Multiple cache queries.
func CachedWorlds(ctx *atreugo.RequestCtx) error {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := 0; i < queries; i++ {
		worlds.W[i] = worldsCache.W[randomWorldNum()-1]
	}

	err := ctx.JSONResponse(worlds.W)
	releaseWorlds(worlds)

	return err
}

// FortunesQuick . Test 5: Fortunes.
func FortunesQuick(ctx *atreugo.RequestCtx) error {
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

	return nil
}

// Updates . Test 6: Database updates.
func Updates(ctx *atreugo.RequestCtx) error {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := 0; i < queries; i++ {
		w := &worlds.W[i]
		db.QueryRow(context.Background(), worldSelectSQL, randomWorldNum()).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
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

	err := ctx.JSONResponse(worlds.W)

	releaseWorlds(worlds)

	return err
}

// Plaintext . Test 7: Plaintext.
func Plaintext(ctx *atreugo.RequestCtx) error {
	return ctx.TextResponse(helloWorldStr)
}
