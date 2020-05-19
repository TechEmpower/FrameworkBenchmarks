package storage

import (
	"sync"

	"github.com/francoispqt/gojay"
	"github.com/tidwall/sjson"
)

var worldJSONStr = []byte(`{"id": 0, "randomNumber": 0}`)

//easyjson:json
type World struct {
	ID           int32 `json:"id"`
	RandomNumber int32 `json:"randomnumber"`
}

// WorldPool ...
var WorldPool = sync.Pool{
	New: func() interface{} {
		return new(World)
	},
}

// AcquireWorld returns new world from pool
func AcquireWorld() *World {
	return WorldPool.Get().(*World)
}

// ReleaseWorld resets the world and return it to the pool
func ReleaseWorld(w *World) {
	w.ID = 0
	w.RandomNumber = 0
	WorldPool.Put(w)
}

// MarshalJSONObject encodes the world as JSON
func (w *World) MarshalJSONObject(dec *gojay.Encoder) {
	dec.AddInt32Key("id", w.ID)
	dec.AddInt32Key("randomnumber", w.RandomNumber)
}

// IsNil returns true if the object is nil
func (w *World) IsNil() bool {
	return w == nil
}

// MarshalSJSON marshals the object as json
func (w World) MarshalSJSON() ([]byte, error) {
	data, _ := sjson.SetBytesOptions(worldJSONStr, "id", w.ID, &sjson.Options{Optimistic: true})

	return sjson.SetBytesOptions(
		data, "randomNumber", w.RandomNumber, &sjson.Options{Optimistic: true, ReplaceInPlace: true},
	)
}

//easyjson:json
type Worlds []World

// WorldsPool ...
var WorldsPool = sync.Pool{
	New: func() interface{} {
		return make(Worlds, 0, 512)
	},
}

// AcquireWorlds returns new worlds from pool
func AcquireWorlds() Worlds {
	return WorldsPool.Get().(Worlds)
}

// ReleaseWorlds resets the worlds and return it to the pool
func ReleaseWorlds(w Worlds) {
	w = w[:0]
	WorldsPool.Put(w)
}

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

// MarshalSJSON marshals the object as json
func (ws Worlds) MarshalSJSON() ([]byte, error) {
	jsonResult := []byte(`[]`)

	for _, w := range ws {
		jsonResult, _ = sjson.SetBytesOptions(jsonResult, "-1", &w, &sjson.Options{Optimistic: true, ReplaceInPlace: true})
	}

	return jsonResult, nil
}
