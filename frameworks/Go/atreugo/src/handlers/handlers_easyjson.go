package handlers

import (
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v11"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = helloWorldStr
	data, err := message.MarshalJSON()

	ctx.SetContentType("application/json")
	ctx.Write(data)

	ReleaseMessage(message)

	return err
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()
		db.GetOneRandomWorld(world)
		data, err := world.MarshalJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorld(world)

		return err
	}
}

// QueriesHandlerEasyJSON . Test 3: Multiple database queries
func QueriesHandlerEasyJSON(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			db.GetOneRandomWorld(&worlds[i])
		}

		data, err := worlds.MarshalJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// UpdateHandlerEasyJSON . Test 5: Database updates
func UpdateHandlerEasyJSON(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			w := &worlds[i]
			db.GetOneRandomWorld(w)
			w.RandomNumber = int32(storage.RandomWorldNum())
		}

		db.UpdateWorlds(worlds)
		data, err := worlds.MarshalJSON()

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)

		return err
	}
}
