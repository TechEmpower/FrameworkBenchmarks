package handlers

import (
	"atreugo/src/storage"
	"log"

	"github.com/savsgio/atreugo/v7"
)

// JSONHandlerSJson . Test 1: JSON serialization
func JSONHandlerSJson(ctx *atreugo.RequestCtx) error {
	message := MessagePool.Get().(*Message)
	message.Message = "Hello, World!"

	ctx.SetContentType("application/json")
	data, err := message.MarshalSJSON()
	if err == nil {
		_, err = ctx.Write(data)
	}

	MessagePool.Put(message)
	return err
}

// DBHandlerSJson . Test 2: Single database query
func DBHandlerSJson(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.WorldPool.Get().(*storage.World)

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		data, err := world.MarshalSJSON()
		if err == nil {
			_, err = ctx.Write(data)
		}

		storage.WorldPool.Put(world)
		return err
	}
}

// QueriesHandlerSJson . Test 3: Multiple database queries
func QueriesHandlerSJson(db storage.DB) func(ctx *atreugo.RequestCtx) error {
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
		data, err := storage.Worlds(worlds).MarshalSJSON()
		if err == nil {
			_, err = ctx.Write(data)
		}

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}

// UpdateHandlerSJson . Test 5: Database updates
func UpdateHandlerSJson(db storage.DB) func(ctx *atreugo.RequestCtx) error {
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
		data, err := storage.Worlds(worlds).MarshalSJSON()
		if err == nil {
			_, err = ctx.Write(data)
		}

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}
