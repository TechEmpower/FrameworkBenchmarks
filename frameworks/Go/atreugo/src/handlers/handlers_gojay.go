package handlers

import (
	"atreugo/src/storage"

	"github.com/francoispqt/gojay"
	"github.com/savsgio/atreugo/v10"
)

// JSONHandlerGoJay . Test 1: JSON serialization
func JSONHandlerGoJay(ctx *atreugo.RequestCtx) error {
	message := AcquireMessage()
	message.Message = helloWorldStr

	ctx.SetContentType("application/json")
	err := gojay.NewEncoder(ctx).Encode(message)

	ReleaseMessage(message)

	return err
}

// DBHandlerGoJay . Test 2: Single database query
func DBHandlerGoJay(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.AcquireWorld()

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).Encode(world)

		storage.ReleaseWorld(world)

		return err
	}
}

// QueriesHandlerGoJay . Test 3: Multiple database queries
func QueriesHandlerGoJay(db storage.DB) atreugo.View {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		worlds := storage.AcquireWorlds()[:queries]

		for i := 0; i < queries; i++ {
			if err := db.GetOneRandomWorld(&worlds[i]); err != nil {
				return err
			}
		}

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).EncodeArray(worlds)

		storage.ReleaseWorlds(worlds)

		return err
	}
}

// UpdateHandlerGoJay . Test 5: Database updates
func UpdateHandlerGoJay(db storage.DB) atreugo.View {
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

		ctx.SetContentType("application/json")
		err := gojay.NewEncoder(ctx).EncodeArray(worlds)

		storage.ReleaseWorlds(worlds)

		return err
	}
}
