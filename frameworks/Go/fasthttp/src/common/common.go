package common

import (
	"encoding/json"
	"log"
	"math/rand"
	"net"
	"sort"

	"fasthttp/src/templates"

	"github.com/valyala/fasthttp"
)

const worldRowCount = 10000

type JSONResponse struct {
	Message string `json:"message"`
}

type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

type Worlds []World

func JSONHandler(ctx *fasthttp.RequestCtx) {
	r := JSONResponse{
		Message: "Hello, World!",
	}
	rb, err := r.MarshalJSON()
	if err != nil {
		log.Println(err)
		return
	}
	ctx.SetContentType("application/json")
	ctx.Write(rb)
}

func PlaintextHandler(ctx *fasthttp.RequestCtx) {
	ctx.SetContentType("text/plain")
	ctx.WriteString("Hello, World!")
}

func JSONMarshal(ctx *fasthttp.RequestCtx, v interface{}) {
	ctx.SetContentType("application/json")
	if err := json.NewEncoder(ctx).Encode(v); err != nil {
		log.Fatalf("error in json.Encoder.Encode: %s", err)
	}
}

func RandomWorldNum() int {
	return rand.Intn(worldRowCount) + 1
}

func GetQueriesCount(ctx *fasthttp.RequestCtx) int {
	n := ctx.QueryArgs().GetUintOrZero("queries")
	if n < 1 {
		n = 1
	} else if n > 500 {
		n = 500
	}
	return n
}

func GetListener(listenAddr string) net.Listener {
	ln, err := net.Listen("tcp4", listenAddr)
	if err != nil {
		log.Fatal(err)
	}
	return ln
}

func SortFortunesByMessage(fortunes []templates.Fortune) {
	sort.Slice(fortunes, func(i, j int) bool { return fortunes[i].Message < fortunes[j].Message })
}

func SortWorldsByID(worlds []World) {
	sort.Slice(worlds, func(i, j int) bool { return worlds[i].Id < worlds[j].Id })
}
