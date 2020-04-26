package handlers

import (
	"fasthttp/src/storage"

	"github.com/valyala/fasthttp"
)

// JSONHandlerSJson . Test 1: JSON serialization
func JSONHandlerSJson(ctx *fasthttp.RequestCtx) {
	message := AcquireMessage()
	message.Message = helloWorldStr

	data, err := message.MarshalSJSON()
	if err != nil {
		ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
		return
	}

	ctx.SetContentType("application/json")
	ctx.Write(data)

	ReleaseMessage(message)
}

// DBHandlerSJson . Test 2: Single database query
func DBHandlerSJson(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		data, err := world.MarshalSJSON()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorld(world)
	}
}

// QueriesHandlerSJson . Test 3: Multiple database queries
func QueriesHandlerSJson(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
				return
			}
		}

		data, err := worlds.MarshalSJSON()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)
	}
}

// UpdateHandlerSJson . Test 5: Database updates
func UpdateHandlerSJson(db storage.DB) fasthttp.RequestHandler {
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

		data, err := worlds.MarshalSJSON()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)
	}
}
