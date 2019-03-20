package storage

import (
	"sync"

	"github.com/francoispqt/gojay"
)

//easyjson:json
type World struct {
	ID           int `json:"id"`
	RandomNumber int `json:"randomnumber"`
}

// MarshalJSONObject encodes the world as JSON
func (w *World) MarshalJSONObject(dec *gojay.Encoder) {
	dec.AddIntKey("id", w.ID)
	dec.AddIntKey("randomnumber", w.RandomNumber)

}

// IsNil returns true if the object is nil
func (w *World) IsNil() bool {
	return w == nil
}

//easyjson:json
type Worlds []World

// MarshalJSONArray marshals the list of worlds
func (ws Worlds) MarshalJSONArray(enc *gojay.Encoder) {
	for _, w := range ws {
		enc.AddObject(&w)
	}
}

// IsNil returns true if the object is nil
func (ws Worlds) IsNil() bool {
	return ws == nil
}

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
