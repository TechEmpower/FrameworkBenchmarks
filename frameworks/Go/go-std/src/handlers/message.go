package handlers

//go:generate go run github.com/mailru/easyjson/... -all -disable_members_unescape ${GOFILE}

import "sync"

// Message struct
type Message struct {
	Message string `json:"message"`
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
