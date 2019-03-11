package handlers

import (
	"encoding/json"
	"log"
	"sort"

	"atreugo/src/storage"
	"atreugo/src/templates"

	"github.com/savsgio/atreugo"
)

func queriesParam(ctx *atreugo.RequestCtx) int {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

// JSONHandler . Test 1: JSON serialization
func JSONHandler(ctx *atreugo.RequestCtx) error {
	message := MessagePool.Get().(*Message)
	message.Message = "Hello, World!"
	ctx.SetContentType("application/json")
	messageBytes, err := json.Marshal(message)
	if err != nil {
		return err
	}
	_, err = ctx.Write(messageBytes)
	MessagePool.Put(message)
	return err
}

// DBHandler . Test 2: Single database query
func DBHandler(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		world := storage.WorldPool.Get().(*storage.World)

		if err := db.GetOneRandomWorld(world); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		worldBytes, err := json.Marshal(world)
		if err != nil {
			return err
		}
		_, err = ctx.Write(worldBytes)

		storage.WorldPool.Put(world)

		return err
	}
}

// QueriesHandler . Test 3: Multiple database queries
func QueriesHandler(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)

		// worlds := make([]storage.World, queries)
		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		var err error
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		ctx.SetContentType("application/json")
		worldsBytes, err := json.Marshal(worlds)
		if err != nil {
			return err
		}
		_, err = ctx.Write(worldsBytes)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}

// FortuneHandler . Test 4: Fortunes
func FortuneHandler(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		fortunes, err := db.GetFortunes()
		if err != nil {
			return err
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")
		return templates.FortuneTemplate.Execute(ctx, fortunes)
	}
}

// FortuneHandlerPool . Test 4: Fortunes
func FortuneHandlerPool(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		fortunes, err := db.GetFortunesPool()
		if err != nil {
			return err
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")
		if err = templates.FortuneTemplate.Execute(ctx, fortunes); err != nil {
			return err
		}

		fortunes = fortunes[:0]
		templates.FortunesPool.Put(fortunes)

		return nil
	}
}

// FortuneQuickHandler . Test 4: Fortunes
func FortuneQuickHandler(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		fortunes, err := db.GetFortunes()
		if err != nil {
			return err
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")
		templates.WriteFortunePage(ctx, fortunes)

		return nil
	}
}

// FortuneQuickHandlerPool . Test 4: Fortunes
func FortuneQuickHandlerPool(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		fortunes, err := db.GetFortunesPool()
		if err != nil {
			return err
		}
		fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})

		sort.Slice(fortunes, func(i, j int) bool {
			return fortunes[i].Message < fortunes[j].Message
		})

		ctx.SetContentType("text/html; charset=utf-8")
		templates.WriteFortunePage(ctx, fortunes)

		fortunes = fortunes[:0]
		templates.FortunesPool.Put(fortunes)

		return nil
	}
}

// UpdateHandler . Test 5: Database updates
func UpdateHandler(db storage.DB) func(ctx *atreugo.RequestCtx) error {
	return func(ctx *atreugo.RequestCtx) error {
		queries := queriesParam(ctx)
		var err error

		// worlds := make([]storage.World, queries)
		worlds := storage.WorldsPool.Get().([]storage.World)[:queries]

		// for _, world := range worlds {
		for i := 0; i < queries; i++ {
			if err = db.GetOneRandomWorld(&worlds[i]); err != nil {
				log.Println(err)
			}
		}

		if err = db.UpdateWorlds(worlds); err != nil {
			return err
		}

		ctx.SetContentType("application/json")
		worldsBytes, err := json.Marshal(worlds)
		if err != nil {
			return err
		}
		_, err = ctx.Write(worldsBytes)

		worlds = worlds[:0]
		storage.WorldsPool.Put(worlds)

		return err
	}
}

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(ctx *atreugo.RequestCtx) error {
	ctx.SetContentType("text/plain")
	_, err := ctx.WriteString("Hello, World!")
	return err
}
