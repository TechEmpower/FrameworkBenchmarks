package handlers

import (
	"sync"

	"github.com/francoispqt/gojay"
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
