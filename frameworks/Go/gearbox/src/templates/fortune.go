package templates

import (
	"sync"
)

type Fortune struct {
	ID      int    `json:"id,omitempty"`
	Message string `json:"message,omitempty"`
}

type Fortunes struct {
	F []Fortune
}

//go:generate qtc

var fortunePool = &sync.Pool{
	New: func() interface{} {
		return new(Fortune)
	},
}

var fortunesPool = &sync.Pool{
	New: func() interface{} {
		return &Fortunes{
			F: make([]Fortune, 0, 16),
		}
	},
}

// AcquireFortune returns new message from pool.
func AcquireFortune() *Fortune {
	return fortunePool.Get().(*Fortune)
}

// ReleaseFortune resets the message and return it to the pool.
func ReleaseFortune(f *Fortune) {
	f.ID = 0
	f.Message = ""
	fortunePool.Put(f)
}

// AcquireFortunes returns new fortunes from pool.
func AcquireFortunes() *Fortunes {
	return fortunesPool.Get().(*Fortunes)
}

// ReleaseFortunes resets the fortunes and return it to the pool.
func ReleaseFortunes(f *Fortunes) {
	f.F = f.F[:0]
	fortunesPool.Put(f)
}
