package handlers

import (
	"log"

	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v10"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *atreugo.RequestCtx) error {
	ctx.SetContentType("application/json")

	message := AcquireMessage()
	message.Message = "Hello, World!"

	messageBytes, err := message.MarshalJSON()
	if err != nil {
		return err
	}

	_, err = ctx.Write(messageBytes)

	ReleaseMessage(message)

	return err
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) func(ctx *atreugo.RequestCtx) error {
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
		_, err = ctx.Write(worldBytes)

		storage.ReleaseWorld(world)

		return err
	}
}

// QueriesHandlerEasyJSON . Test 3: Multiple database queries
func QueriesHandlerEasyJSON(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		worldsBytes, err := worlds.MarshalJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		_, err = ctx.Write(worldsBytes)

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// UpdateHandlerEasyJSON . Test 5: Database updates
func UpdateHandlerEasyJSON(db storage.DB) func(ctx *atreugo.RequestCtx) error {
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

		worldsBytes, err := worlds.MarshalJSON()
		if err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		_, err = ctx.Write(worldsBytes)

		storage.ReleaseWorlds(worlds)

		return err
	}
}
