package handlers

import (
	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v10"
)

// JSONHandlerSJson . Test 1: JSON serialization
func JSONHandlerSJson(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = helloWorldStr

	data, err := message.MarshalSJSON()
	if err != nil {
		return err
	}

	ctx.SetContentType("application/json")
	ctx.Write(data)

	ReleaseMessage(message)

	return nil
}

// DBHandlerSJson . Test 2: Single database query
func DBHandlerSJson(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		data, err := world.MarshalSJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorld(world)

		return nil
	}
}

// QueriesHandlerSJson . Test 3: Multiple database queries
func QueriesHandlerSJson(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				return err
			}
		}

		data, err := worlds.MarshalSJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)

		return nil
	}
}

// UpdateHandlerSJson . Test 5: Database updates
func UpdateHandlerSJson(db storage.DB) atreugo.View {
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

		data, err := worlds.MarshalSJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		ctx.Write(data)

		storage.ReleaseWorlds(worlds)

		return err
	}
}
