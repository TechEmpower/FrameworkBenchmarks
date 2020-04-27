package handlers

import (
	"sync"

	"github.com/tidwall/sjson"
)

// Message struct
type Message struct {
	Message string `json:"message"`
}

var messageJSONStr = []byte(`{"message": ""}`)

// MessagePool ...
var MessagePool = sync.Pool{
	New: func() interface{} {
		return new(Message)
	},
}

// AcquireMessage returns new message from pool
func AcquireMessage() *Message {
	return MessagePool.Get().(*Message)
}

// ReleaseMessage resets the message and return it to the pool
func ReleaseMessage(m *Message) {
	m.Message = ""
	MessagePool.Put(m)
}

// IsNil returns true if the object is nil
func (m *Message) IsNil() bool {
	return m == nil
}

// MarshalSJSON marshals the object as json
func (m Message) MarshalSJSON() ([]byte, error) {
	return sjson.SetBytesOptions(messageJSONStr, "message", m.Message, &sjson.Options{Optimistic: true})
}
