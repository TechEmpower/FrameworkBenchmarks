package handlers

import (
	"sync"
)

var messagePool = sync.Pool{
	New: func() interface{} {
		return new(Message)
	},
}

func acquireMessage() *Message {
	return messagePool.Get().(*Message)
}

func releaseMessage(m *Message) {
	m.Message = ""
	messagePool.Put(m)
}
