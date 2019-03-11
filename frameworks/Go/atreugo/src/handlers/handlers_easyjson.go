package handlers

import (
	"github.com/savsgio/atreugo"
)

// JSONHandlerEasyJSON . Test 1: JSON serialization
func JSONHandlerEasyJSON(ctx *atreugo.RequestCtx) error {
	message := MessagePool.Get().(*Message)
	ctx.SetContentType("application/json")
	message.Message = "Hello, World!"
	messageBytes, err := message.MarshalJSON()
	if err != nil {
		return err
	}
	_, err = ctx.Write(messageBytes)
	MessagePool.Put(message)
	return err
}
