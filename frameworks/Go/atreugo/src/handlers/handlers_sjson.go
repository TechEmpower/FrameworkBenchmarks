package handlers

import (
	"log"

	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v10"
)


// JSONHandlerSJson . Test 1: JSON serialization
func JSONHandlerSJson(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = "Hello, World!"

	ctx.SetContentType("application/json")
	data, err := message.MarshalSJSON()
	if err == nil {
		_, err = ctx.Write(data)
	}

	ReleaseMessage(message)

	return err
}

// DBHandlerSJson . Test 2: Single database query
func DBHandlerSJson(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		data, err := world.MarshalSJSON()
		if err == nil {
			_, err = ctx.Write(data)
		}

		storage.ReleaseWorld(world)

		return err
	}
}

// QueriesHandlerSJson . Test 3: Multiple database queries
func QueriesHandlerSJson(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		ctx.SetContentType("application/json")
		data, err := worlds.MarshalSJSON()
		if err == nil {
			_, err = ctx.Write(data)
		}

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// UpdateHandlerSJson . Test 5: Database updates
func UpdateHandlerSJson(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorlds(worlds); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		data, err := worlds.MarshalSJSON()
		if err == nil {
			_, err = ctx.Write(data)
		}

		storage.ReleaseWorlds(worlds)

		return err
	}
}
