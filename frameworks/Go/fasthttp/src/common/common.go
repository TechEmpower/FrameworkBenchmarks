package common

import (
	"encoding/json"
	"flag"
	"log"
	"math/rand"
	"net"
	"sync"

	"github.com/valyala/fasthttp"

	"templates"
)

const worldRowCount = 10000

type JSONResponse struct {
	Message string `json:"message"`
}

type World struct {
	Id           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

var listenAddr = flag.String("listenAddr", ":8080", "Address to listen to")

func JSONHandler(ctx *fasthttp.RequestCtx) {
	r := jsonResponsePool.Get().(*JSONResponse)
	r.Message = "Hello, World!"
	JSONMarshal(ctx, r)
	jsonResponsePool.Put(r)
}

var jsonResponsePool = &sync.Pool{
	New: func() interface{} {
		return &JSONResponse{}
	},
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

type FortunesByMessage []templates.Fortune

func (s FortunesByMessage) Len() int           { return len(s) }
func (s FortunesByMessage) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s FortunesByMessage) Less(i, j int) bool { return s[i].Message < s[j].Message }

type WorldsByID []World

func (w WorldsByID) Len() int           { return len(w) }
func (w WorldsByID) Swap(i, j int)      { w[i], w[j] = w[j], w[i] }
func (w WorldsByID) Less(i, j int) bool { return w[i].Id < w[j].Id }

func GetListener() net.Listener {
	ln, err := net.Listen("tcp4", *listenAddr)
	if err != nil {
		log.Fatal(err)
	}
	return ln
}
