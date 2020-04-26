package handlers

import (
	"encoding/json"
	"sort"

	"fasthttp/src/storage"
	"fasthttp/src/templates"

	"github.com/valyala/fasthttp"
)

const helloWorldStr = "Hello, World!"

func queriesParam(ctx *fasthttp.RequestCtx) int {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	return n
}

// JSONHandler . Test 1: JSON serialization
func JSONHandler(ctx *fasthttp.RequestCtx) {
	message := AcquireMessage()
	message.Message = helloWorldStr

	data, err := json.Marshal(message)
	if err != nil {
		ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
		return
	}

	ctx.SetContentType("application/json")
	ctx.Write(data)

	ReleaseMessage(message)
}

// DBHandler . Test 2: Single database query
func DBHandler(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		data, err := json.Marshal(world)
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorld(world)
	}
}

// QueriesHandler . Test 3: Multiple database queries
func QueriesHandler(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		queries := queriesParam(ctx)

		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
				return
			}
		}

		data, err := json.Marshal(worlds)
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)
	}
}

// FortuneHandler . Test 4: Fortunes
func FortuneHandler(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		fortunes, err := db.GetFortunes()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		newFortune := templates.AcquireFortune()
		newFortune.Message = "Additional fortune added at request time."

		fortunes = append(fortunes, *newFortune)

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")

		if err = templates.FortuneTemplate.Execute(ctx, fortunes); err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		templates.ReleaseFortune(newFortune)
		templates.ReleaseFortunes(fortunes)
	}
}

// FortuneQuickHandler . Test 4: Fortunes
func FortuneQuickHandler(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		fortunes, err := db.GetFortunes()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
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
	}
}

// UpdateHandler . Test 5: Database updates
func UpdateHandler(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			w := &worlds[i]
			if err := db.GetOneRandomWorld(w); err != nil {
				ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
				return
			}
			w.RandomNumber = int32(storage.RandomWorldNum())
		}

		if err := db.UpdateWorlds(worlds); err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		data, err := json.Marshal(worlds)
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)
	}
}

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(ctx *fasthttp.RequestCtx) {
	ctx.WriteString(helloWorldStr)
}
