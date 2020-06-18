package views

import (
	"sort"

	"atreugo/src/templates"

	pgx "github.com/jackc/pgx/v4"
	"github.com/savsgio/atreugo/v11"
)

const helloWorldStr = "Hello, World!"

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
	id := randomWorldNum()

	db.QueryRow(ctx, worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
	err := ctx.JSONResponse(w)

	releaseWorld(w)

	return err
}

// Queries . Test 3: Multiple database queries.
func Queries(ctx *atreugo.RequestCtx) error {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := range worlds.W {
		w := &worlds.W[i]
		id := randomWorldNum()

		db.QueryRow(ctx, worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
	}

	err := ctx.JSONResponse(worlds.W)

	releaseWorlds(worlds)

	return err
}

// FortuneQuick . Test 4: Fortunes.
func FortuneQuick(ctx *atreugo.RequestCtx) error {
	fortune := templates.AcquireFortune()
	fortunes := templates.AcquireFortunes()

	rows, _ := db.Query(ctx, fortuneSelectSQL)
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

	ctx.Response.Header.SetContentType("text/html; charset=utf-8")
	templates.WriteFortunePage(ctx, fortunes.F)

	templates.ReleaseFortune(fortune)
	templates.ReleaseFortunes(fortunes)

	return nil
}

// Update . Test 5: Database updates.
func Update(ctx *atreugo.RequestCtx) error {
	queries := queriesParam(ctx)
	worlds := acquireWorlds()
	worlds.W = worlds.W[:queries]

	for i := range worlds.W {
		w := &worlds.W[i]
		id := randomWorldNum()

		db.QueryRow(ctx, worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber) // nolint:errcheck
		w.RandomNumber = int32(randomWorldNum())
	}

	// against deadlocks
	sort.Slice(worlds.W, func(i, j int) bool {
		return worlds.W[i].ID < worlds.W[j].ID
	})

	batch := &pgx.Batch{}

	for i := range worlds.W {
		w := &worlds.W[i]
		batch.Queue(worldUpdateSQL, w.RandomNumber, w.ID)
	}

	db.SendBatch(ctx, batch).Close()
	err := ctx.JSONResponse(worlds.W)

	releaseWorlds(worlds)

	return err
}

// Plaintext . Test 6: Plaintext.
func Plaintext(ctx *atreugo.RequestCtx) error {
	return ctx.TextResponse(helloWorldStr)
}
