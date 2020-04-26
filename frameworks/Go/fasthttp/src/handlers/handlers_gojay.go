package handlers

import (
	"fasthttp/src/storage"

	"github.com/francoispqt/gojay"
	"github.com/valyala/fasthttp"
)

// JSONHandlerGoJay . Test 1: JSON serialization
func JSONHandlerGoJay(ctx *fasthttp.RequestCtx) {
	message := AcquireMessage()
	message.Message = helloWorldStr

	ctx.SetContentType("application/json")
	err := gojay.NewEncoder(ctx).Encode(message)

	ReleaseMessage(message)

	if err != nil {
		ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
	}
}

// DBHandlerGoJay . Test 2: Single database query
func DBHandlerGoJay(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).Encode(world)

		storage.ReleaseWorld(world)

		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
		}
	}
}

// QueriesHandlerGoJay . Test 3: Multiple database queries
func QueriesHandlerGoJay(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
				return
			}
		}

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).EncodeArray(worlds)

		storage.ReleaseWorlds(worlds)

		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
		}
	}
}

// UpdateHandlerGoJay . Test 5: Database updates
func UpdateHandlerGoJay(db storage.DB) fasthttp.RequestHandler {
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

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).EncodeArray(worlds)

		storage.ReleaseWorlds(worlds)

		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
		}
	}
}
