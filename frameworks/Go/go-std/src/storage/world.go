package storage

import "sync"

//easyjson:json
type World struct {
	ID           int `json:"id"`
	RandomNumber int `json:"randomnumber"`
}

//easyjson:json
type Worlds []World

var WorldPool *sync.Pool

// InitWorldPool ()
func InitWorldPool() {
	WorldPool = &sync.Pool{
		New: func() interface{} {
			return &World{}
		},
	}
}
