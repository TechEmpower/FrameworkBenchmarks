package handlers

import "sync"

// Message struct
type Message struct {
	Message string `json:"message"`
}

var MessagePool *sync.Pool

// InitMessagePool ()
func InitMessagePool() {
	MessagePool = &sync.Pool{
		New: func() interface{} {
			return &Message{}
		},
	}
}
