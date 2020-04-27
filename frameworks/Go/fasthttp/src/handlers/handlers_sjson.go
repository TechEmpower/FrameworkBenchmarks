package handlers

import (
	"fasthttp/src/storage"

	"github.com/valyala/fasthttp"
)

// JSONHandlerSJson . Test 1: JSON serialization
func JSONHandlerSJson(ctx *fasthttp.RequestCtx) {
	message := AcquireMessage()
	message.Message = helloWorldStr
	data, _ := message.MarshalSJSON()

	ctx.SetContentType("application/json")
	ctx.Write(data)

	ReleaseMessage(message)
}

// DBHandlerSJson . Test 2: Single database query
func DBHandlerSJson(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		world := storage.AcquireWorld()
		db.GetOneRandomWorld(world)
		data, _ := world.MarshalSJSON()

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
			db.GetOneRandomWorld(&worlds[i])
		}

		data, _ := worlds.MarshalSJSON()

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
			db.GetOneRandomWorld(w)
			w.RandomNumber = int32(storage.RandomWorldNum())
		}

		db.UpdateWorlds(worlds)
		data, _ := worlds.MarshalSJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)
	}
}
