package handlers

import (
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v11"
)

// JSONHandlerSJson . Test 1: JSON serialization
func JSONHandlerSJson(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = helloWorldStr
	data, err := message.MarshalSJSON()

	ctx.SetContentType("application/json")
	ctx.Write(data)

	ReleaseMessage(message)

	return err
}

// DBHandlerSJson . Test 2: Single database query
func DBHandlerSJson(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()
		db.GetOneRandomWorld(world)
		data, err := world.MarshalSJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorld(world)

		return err
	}
}

// QueriesHandlerSJson . Test 3: Multiple database queries
func QueriesHandlerSJson(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			db.GetOneRandomWorld(&worlds[i])
		}

		data, err := worlds.MarshalSJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// UpdateHandlerSJson . Test 5: Database updates
func UpdateHandlerSJson(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			w := &worlds[i]
			db.GetOneRandomWorld(w)
			w.RandomNumber = int32(storage.RandomWorldNum())
		}

		db.UpdateWorlds(worlds)
		data, err := worlds.MarshalSJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)

		return err
	}
}
