package handlers

import (
	"sync"

	"github.com/francoispqt/gojay"
	"github.com/tidwall/sjson"
)

// Message struct
type Message struct {
	Message string `json:"message"`
}

// MarshalJSONObject encodes the message as JSON
func (m *Message) MarshalJSONObject(dec *gojay.Encoder) {
	dec.AddStringKey("message", m.Message)
}

// IsNil returns true if the object is nil
func (m *Message) IsNil() bool {
	return m == nil
}

// MarshalSJSON marshals the object as json
func (m Message) MarshalSJSON() ([]byte, error) {
	return sjson.SetBytesOptions([]byte(`{"message": ""}`), "message", m.Message, &sjson.Options{Optimistic: true})
	// &sjson.Options{Optimistic: true}
}

// MessagePool *sync.Pool
var MessagePool *sync.Pool

// InitMessagePool ()
func InitMessagePool() {
	MessagePool = &sync.Pool{
		New: func() interface{} {
			return &Message{}
		},
	}
}
