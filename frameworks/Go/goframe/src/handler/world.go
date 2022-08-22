package handler

import (
	"sync"
)

const (
	maxWorlds   = 500
	worldsCount = 10000
)

var (
	worldPool = sync.Pool{
		New: func() interface{} {
			return new(World)
		},
	}
	worldsPool = sync.Pool{
		New: func() interface{} {
			return &Worlds{
				W: make([]World, 0, maxWorlds),
			}
		},
	}
)

func acquireWorld() *World {
	return worldPool.Get().(*World)
}

func releaseWorld(w *World) {
	w.ID = 0
	w.RandomNumber = 0
	worldPool.Put(w)
}

func acquireWorlds() *Worlds {
	return worldsPool.Get().(*Worlds)
}

func releaseWorlds(w *Worlds) {
	w.W = w.W[:0]
	worldsPool.Put(w)
}
