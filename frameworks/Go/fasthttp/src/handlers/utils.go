package handlers

import (
	"math/rand"

	"github.com/valyala/fasthttp"
)

func queriesParam(ctx *fasthttp.RequestCtx) int {
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
