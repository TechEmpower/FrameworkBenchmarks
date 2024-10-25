package controllers

import (
	"math/rand/v2"
	"sort"

	"github.com/goravel/framework/contracts/http"
	"github.com/goravel/framework/facades"

	"goravel/app/models"
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
	world := acquireWorld()

	world.ID = r.getRand()
	_ = facades.Orm().Query().Find(&world)

	JSON(ctx, &world)
	releaseWorld(world)
	return nil
}

func (r *TestController) Queries(ctx http.Context) http.Response {
	n := r.getN(ctx)
	worlds := acquireWorlds()[:n]

	for i := 0; i < n; i++ {
		worlds[i].ID = r.getRand()
		_ = facades.Orm().Query().Find(&worlds[i])
	}

	JSON(ctx, &worlds)
	releaseWorlds(worlds)
	return nil
}

func (r *TestController) Update(ctx http.Context) http.Response {
	n := r.getN(ctx)
	worlds := acquireWorlds()[:n]

	for i := 0; i < n; i++ {
		worlds[i].ID = r.getRand()
		_ = facades.Orm().Query().Find(&worlds[i])
	}

	// sorting is required for insert deadlock prevention.
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].ID < worlds[j].ID
	})

	tx, _ := facades.Orm().Query().Begin()
	for i := 0; i < n; i++ {
		worlds[i].RandomNumber = r.getRand()
		_ = tx.Save(&worlds[i])
	}
	_ = tx.Commit()

	JSON(ctx, &worlds)
	releaseWorlds(worlds)
	return nil
}

func (r *TestController) Fortunes(ctx http.Context) http.Response {
	fortunes := make([]models.Fortune, 0)
	_ = facades.Orm().Query().Get(&fortunes)
	fortunes = append(fortunes, models.Fortune{Message: "Additional fortune added at request time."})

	sort.Slice(fortunes, func(i, j int) bool {
		return fortunes[i].Message < fortunes[j].Message
	})

	return ctx.Response().
		Header("Server", "Goravel").
		View().
		Make("fortunes.tmpl", map[string]any{
			"fortunes": fortunes,
		})
}

func (r *TestController) CacheQueries(ctx http.Context) http.Response {
	n := r.getN(ctx)
	worlds := acquireWorlds()[:n]
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
