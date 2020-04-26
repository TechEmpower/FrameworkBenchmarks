package handlers

import (
	"fasthttp/src/storage"

	"github.com/valyala/fasthttp"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *fasthttp.RequestCtx) {
	message := AcquireMessage()
	message.Message = helloWorldStr

	messageBytes, err := message.MarshalJSON()
	if err != nil {
		ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
		return
	}

	ctx.SetContentType("application/json")
	ctx.Write(messageBytes)

	ReleaseMessage(message)
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		worldBytes, err := world.MarshalJSON()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(worldBytes)

		storage.ReleaseWorld(world)
	}
}

// QueriesHandlerEasyJSON . Test 3: Multiple database queries
func QueriesHandlerEasyJSON(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
				return
			}
		}

		worldsBytes, err := worlds.MarshalJSON()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(worldsBytes)

		storage.ReleaseWorlds(worlds)
	}
}

// UpdateHandlerEasyJSON . Test 5: Database updates
func UpdateHandlerEasyJSON(db storage.DB) fasthttp.RequestHandler {
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

		worldsBytes, err := worlds.MarshalJSON()
		if err != nil {
			ctx.Error(err.Error(), fasthttp.StatusInternalServerError)
			return
		}

		ctx.SetContentType("application/json")
		ctx.Write(worldsBytes)

		storage.ReleaseWorlds(worlds)
	}
}
