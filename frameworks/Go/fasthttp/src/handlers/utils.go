package handlers

import (
	"math/rand"

	"github.com/valyala/fasthttp"
)

const worldsCount = 10000

func queriesParam(ctx *fasthttp.RequestCtx) int {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}

	return n
}

func randomWorldNum() int {
	return rand.Intn(worldsCount) + 1
}
