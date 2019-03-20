package handlers

import (
	"log"

	"github.com/francoispqt/gojay"

	"atreugo/src/storage"

	"github.com/savsgio/atreugo/v7"
)

// JSONHandlerGoJay . Test 1: JSON serialization
func JSONHandlerGoJay(ctx *atreugo.RequestCtx) error {
	message := MessagePool.Get().(*Message)
	message.Message = "Hello, World!"

	ctx.SetContentType("application/json")
	err := gojay.NewEncoder(ctx).Encode(message)

	MessagePool.Put(message)
	return err
}

// DBHandlerGoJay . Test 2: Single database query
func DBHandlerGoJay(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.WorldPool.Get().(*storage.World)

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).Encode(world)

		storage.WorldPool.Put(world)
		return err
	}
}

// QueriesHandlerGoJay . Test 3: Multiple database queries
func QueriesHandlerGoJay(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)

		// worlds := make([]storage.World, queries)
		worlds := storage.WorldsPool.Get().(storage.Worlds)[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		ctx.SetContentType("application/json")
		err = gojay.NewEncoder(ctx).EncodeArray(worlds)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}

// UpdateHandlerGoJay . Test 5: Database updates
func UpdateHandlerGoJay(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		var err error

		worlds := storage.WorldsPool.Get().(storage.Worlds)[:queries]

		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorlds(worlds); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		err = gojay.NewEncoder(ctx).EncodeArray(worlds)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}
