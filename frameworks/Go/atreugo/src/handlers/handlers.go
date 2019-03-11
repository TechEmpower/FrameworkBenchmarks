package handlers

import (
	"encoding/json"

	"github.com/savsgio/atreugo"
)

// JSONHandler . Test 1: JSON serialization
func JSONHandler(ctx *atreugo.RequestCtx) error {
	message := MessagePool.Get().(*Message)
	message.Message = "Hello, World!"
	ctx.SetContentType("application/json")
	message_bytes, err := json.Marshal(message)
	if err != nil {
		return err
	}
	_, err = ctx.Write(message_bytes)
	MessagePool.Put(message)
	return err
}

// PlaintextHandler . Test 6: Plaintext
func PlaintextHandler(ctx *atreugo.RequestCtx) error {
	ctx.SetContentType("text/plain")
	_, err := ctx.WriteString("Hello, World!")
	return err
}
