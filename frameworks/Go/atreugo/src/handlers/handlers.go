package handlers

import (
	"log"
	"sort"

	"atreugo/src/storage"
	"atreugo/src/templates"

	"github.com/savsgio/atreugo/v10"
)

func queriesParam(ctx *atreugo.RequestCtx) int {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

// JSONHandler . Test 1: JSON serialization
func JSONHandler(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = "Hello, World!"

	err := ctx.JSONResponse(message)

	ReleaseMessage(message)

	return err
}

// DBHandler . Test 2: Single database query
func DBHandler(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		err := ctx.JSONResponse(world)

		storage.ReleaseWorld(world)

		return err
	}
}

// QueriesHandler . Test 3: Multiple database queries
func QueriesHandler(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)

		worlds := storage.AcquireWorlds()[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		err = ctx.JSONResponse(worlds)

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// FortuneHandler . Test 4: Fortunes
func FortuneHandler(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		fortunes, err := db.GetFortunes()
		if err != nil {
			return err
		}

		newFortune := templates.AcquireFortune()
		newFortune.Message = "Additional fortune added at request time."

		fortunes = append(fortunes, *newFortune)

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")
		if err = templates.FortuneTemplate.Execute(ctx, fortunes); err != nil {
			return err
		}

		templates.ReleaseFortune(newFortune)
		templates.ReleaseFortunes(fortunes)

		return nil
	}
}

// FortuneQuickHandler . Test 4: Fortunes
func FortuneQuickHandler(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		fortunes, err := db.GetFortunes()
		if err != nil {
			return err
		}

		newFortune := templates.AcquireFortune()
		newFortune.Message = "Additional fortune added at request time."

		fortunes = append(fortunes, *newFortune)

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")
		templates.WriteFortunePage(ctx, fortunes)

		templates.ReleaseFortune(newFortune)
		templates.ReleaseFortunes(fortunes)

		return nil
	}
}

// UpdateHandler . Test 5: Database updates
func UpdateHandler(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)

		var err error

		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorlds(worlds); err != nil {
			return err
		}

		err = ctx.JSONResponse(worlds)

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(ctx *atreugo.RequestCtx) error {
	return ctx.TextResponse("Hello, World!")
}
