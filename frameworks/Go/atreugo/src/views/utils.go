package views

import (
	"math/rand"

	"github.com/savsgio/atreugo/v11"
)

func queriesParam(ctx *atreugo.RequestCtx) int {
	n := ctx.Request.URI().QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > maxWorlds {
		n = maxWorlds
	}

	return n
}

func randomWorldNum() int {
	return rand.Intn(worldsCount) + 1
}
