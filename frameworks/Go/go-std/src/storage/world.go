package storage

import "sync"

//easyjson:json
type World struct {
	ID           int `json:"id"`
	RandomNumber int `json:"randomnumber"`
}

//easyjson:json
type Worlds []World

// WorldPool *sync.Pool
var WorldPool *sync.Pool

// InitWorldPool ()
func InitWorldPool() {
	WorldPool = &sync.Pool{
		New: func() interface{} {
			return &World{}
		},
	}
}

// WorldsPool *sync.Pool
var WorldsPool *sync.Pool

// InitWorldsPool ()
func InitWorldsPool() {
	WorldsPool = &sync.Pool{
		New: func() interface{} {
			return make([]World, 0, 512)
		},
	}
}
