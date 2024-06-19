package controllers

import (
	"fmt"
	"math/rand/v2"
	"sort"

	"github.com/goravel/framework/contracts/http"
	"github.com/goravel/framework/facades"

	"goravel/templates"
)

type TestController struct{}

func NewTestController() *TestController {
	initCache()
	return &TestController{}
}

func (r *TestController) Plaintext(ctx http.Context) http.Response {
	Plaintext(ctx, helloworld)
	return nil
}

func (r *TestController) JSON(ctx http.Context) http.Response {
	message := acquireMessage()
	message.Message = helloworld

	JSON(ctx, &message)
	releaseMessage(message)
	return nil
}

func (r *TestController) DB(ctx http.Context) http.Response {
	randID := r.getRand()
	world := acquireWorld()

	if err := facades.Orm().Query().Where("id", randID).First(&world); err != nil {
		Error(ctx, err)
		return nil
	}

	JSON(ctx, &world)
	releaseWorld(world)
	return nil
}

func (r *TestController) Queries(ctx http.Context) http.Response {
	n := r.getN(ctx)
	worlds := acquireWorlds()[:n]

	for i := 0; i < n; i++ {
		randID := r.getRand()
		if err := facades.Orm().Query().Where("id", randID).Get(&worlds[i]); err != nil {
			Error(ctx, err)
			return nil
		}
	}

	JSON(ctx, &worlds)
	releaseWorlds(worlds)
	return nil
}

func (r *TestController) Update(ctx http.Context) http.Response {
	n := r.getN(ctx)
	worlds := acquireWorlds()[:n]

	for i := 0; i < n; i++ {
		randID := r.getRand()
		if err := facades.Orm().Query().Where("id", randID).Get(&worlds[i]); err != nil {
			Error(ctx, err)
			return nil
		}
	}

	// sorting is required for insert deadlock prevention.
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].ID < worlds[j].ID
	})

	tx, err := facades.Orm().Query().Begin()

	for i := 0; i < n; i++ {
		worlds[i].RandomNumber = r.getRand()
		if err = tx.Save(&worlds[i]); err != nil {
			Error(ctx, err)
			return nil
		}
	}

	if err = tx.Commit(); err != nil {
		Error(ctx, err)
		return nil
	}

	JSON(ctx, &worlds)
	releaseWorlds(worlds)
	return nil
}

func (r *TestController) Fortunes(ctx http.Context) http.Response {
	fortunes := make([]templates.Fortune, 0)
	if err := facades.Orm().Query().Table("Fortune").Get(&fortunes); err != nil {
		Error(ctx, err)
		return nil
	}

	fortunes = append(fortunes, templates.Fortune{Message: "Additional fortune added at request time."})
	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})

	HTML(ctx)
	templates.WriteFortunePage(ctx.Response().Writer(), fortunes)
	return nil
}

func (r *TestController) CacheQueries(ctx http.Context) http.Response {
	n := r.getN(ctx)
	worlds := acquireWorlds()[:n]

	if !facades.Cache().Has("worlds") {

		if err := facades.Orm().Query().Get(&worlds); err != nil {
			panic(fmt.Sprintf("Failed to init cached Worlds: %v", err))
		}

		facades.Cache().Forever("worlds", worlds)
	}

	cached := facades.Cache().Get("worlds").(Worlds)

	for i := 0; i < n; i++ {
		worlds[i] = cached[r.getRand()-1]
	}

	JSON(ctx, &worlds)
	releaseWorlds(worlds)
	return nil
}

func (r *TestController) getN(ctx http.Context) int {
	n := ctx.Request().QueryInt(queryparam)
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	return n
}

func (r *TestController) getRand() int32 {
	return rand.Int32N(worldcount) + 1
}
