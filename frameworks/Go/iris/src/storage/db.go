package storage

import (
	"iris/src/templates"
)

const (
	selectQueryStr  = "SELECT id, randomNumber FROM World WHERE id = $1"
	updateQueryStr  = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneQueryStr = "SELECT id, message FROM Fortune"
)

// DB is interface for
type DB interface {
	GetOneRandomWorld() (World, error)
	UpdateRandomWorlds(queries int) ([]World, error)
	GetFortunes() ([]templates.Fortune, error)
	Close()
}
