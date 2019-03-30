package handlers

import (
	"atreugo/src/storage"
	"log"

	"github.com/savsgio/atreugo/v7"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *atreugo.RequestCtx) error {
	message := MessagePool.Get().(*Message)
	ctx.SetContentType("application/json")
	message.Message = "Hello, World!"
	messageBytes, err := message.MarshalJSON()
	if err != nil {
		return err
	}
	_, err = ctx.Write(messageBytes)
	MessagePool.Put(message)
	return err
}

// DBHandlerEasyJSON . Test 2: Single database query
func DBHandlerEasyJSON(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.WorldPool.Get().(*storage.World)

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		worldBytes, err := world.MarshalJSON()
		if err != nil {
			return err
		}
		_, err = ctx.Write(worldBytes)

		storage.WorldPool.Put(world)

		return err
	}
}

// QueriesHandlerEasyJSON . Test 3: Multiple database queries
func QueriesHandlerEasyJSON(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)

		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		ctx.SetContentType("application/json")
		worldsBytes, err := storage.Worlds(worlds).MarshalJSON()
		if err != nil {
			return err
		}
		_, err = ctx.Write(worldsBytes)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}

// UpdateHandlerEasyJSON . Test 5: Database updates
func UpdateHandlerEasyJSON(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		var err error

		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorlds(worlds); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		worldsBytes, err := storage.Worlds(worlds).MarshalJSON()
		if err != nil {
			return err
		}
		_, err = ctx.Write(worldsBytes)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}
