package main

import "github.com/go-faster/jx"

func (m Message) JSONEncode(e *jx.Encoder) {
	m.Encode(e)
}
