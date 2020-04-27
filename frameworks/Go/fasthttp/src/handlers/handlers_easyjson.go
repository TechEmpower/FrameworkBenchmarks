package handlers

import (
	"fasthttp/src/storage"

	"github.com/valyala/fasthttp"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *fasthttp.RequestCtx) {
	message := AcquireMessage()
	message.Message = helloWorldStr
	messageBytes, _ := message.MarshalJSON()

	ctx.SetContentType("application/json")
	ctx.Write(messageBytes)

	ReleaseMessage(message)
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) fasthttp.RequestHandler {
	return func(ctx *fasthttp.RequestCtx) {
		world := storage.AcquireWorld()
		db.GetOneRandomWorld(world)
		worldBytes, _ := world.MarshalJSON()

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
			db.GetOneRandomWorld(&worlds[i])
		}

		worldsBytes, _ := worlds.MarshalJSON()

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
			db.GetOneRandomWorld(w)
			w.RandomNumber = int32(storage.RandomWorldNum())
		}

		db.UpdateWorlds(worlds)
		worldsBytes, _ := worlds.MarshalJSON()

		ctx.SetContentType("application/json")
		ctx.Write(worldsBytes)

		storage.ReleaseWorlds(worlds)
	}
}
