package controllers

import (
	"fmt"
	"sync"
	"unsafe"

	"github.com/bytedance/sonic"
	"github.com/goravel/framework/contracts/http"
	"github.com/goravel/framework/facades"

	"goravel/app/models"
)

const (
	queryparam       = "q"
	helloworld       = "Hello, World!"
	worldcount       = 10000
	contentTypePlain = "text/plain; charset=utf-8"
	contentTypeJson  = "application/json"
)

type Message struct {
	Message string `json:"message"`
}

type Worlds []models.World

var messagePool = sync.Pool{
	New: func() any {
		return new(Message)
	},
}

var worldPool = sync.Pool{
	New: func() any {
		return new(models.World)
	},
}

var worldsPool = sync.Pool{
	New: func() any {
		return make(Worlds, 0, 500)
	},
}

func acquireMessage() *Message {
	return messagePool.Get().(*Message)
}

func releaseMessage(m *Message) {
	m.Message = ""
	messagePool.Put(m)
}

func acquireWorld() *models.World {
	return worldPool.Get().(*models.World)
}

func releaseWorld(w *models.World) {
	w.ID = 0
	w.RandomNumber = 0
	worldPool.Put(w)
}

func acquireWorlds() Worlds {
	return worldsPool.Get().(Worlds)
}

func releaseWorlds(w Worlds) {
	w = w[:0]
	worldsPool.Put(w)
}

func str2bytes(s string) []byte {
	return unsafe.Slice(unsafe.StringData(s), len(s))
}

func initCache() {
	worlds := acquireWorlds()
	defer releaseWorlds(worlds)

	if err := facades.Orm().Query().Get(&worlds); err != nil {
		panic(fmt.Sprintf("Failed to init cached Worlds: %v", err))
	}

	facades.Cache().Forever("worlds", worlds)
}

func JSON(ctx http.Context, data any) {
	ctx.Response().Header("Server", "Goravel")
	ctx.Response().Header("Content-Type", contentTypeJson)
	bytes, _ := sonic.Marshal(data)
	_, _ = ctx.Response().Writer().Write(bytes)
}

func Plaintext(ctx http.Context, data string) {
	ctx.Response().Header("Server", "Goravel")
	ctx.Response().Header("Content-Type", contentTypePlain)
	_, _ = ctx.Response().Writer().Write(str2bytes(data))
}

func Error(ctx http.Context, err error) {
	ctx.Response().Header("Server", "Goravel")
	ctx.Response().Header("Content-Type", contentTypePlain)
	ctx.Response().Status(http.StatusInternalServerError)
	_, _ = ctx.Response().Writer().Write(str2bytes(err.Error()))
}
