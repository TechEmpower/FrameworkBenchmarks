package handlers

import (
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v10"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = helloWorldStr

	messageBytes, err := message.MarshalJSON()
	if err != nil {
		return err
	}

	ctx.SetContentType("application/json")
	ctx.Write(messageBytes)

	ReleaseMessage(message)

	return err
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		worldBytes, err := world.MarshalJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		ctx.Write(worldBytes)

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
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				return err
			}
		}

		worldsBytes, err := worlds.MarshalJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		ctx.Write(worldsBytes)

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
			if err := db.GetOneRandomWorld(w); err != nil {
				return err
			}
			w.RandomNumber = int32(storage.RandomWorldNum())
		}

		if err := db.UpdateWorlds(worlds); err != nil {
			return err
		}

		worldsBytes, err := worlds.MarshalJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		ctx.Write(worldsBytes)

		storage.ReleaseWorlds(worlds)

		return err
	}
}
