package storage

import (
	"errors"
	"math/rand"

	"atreugo/src/templates"
)

const (
	worldsCount = 10000

	worldSelectSQL   = "SELECT id, randomNumber FROM World WHERE id = $1"
	worldUpdateSQL   = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneSelectSQL = "SELECT id, message FROM Fortune"
)

// DB is interface for
type DB interface {
	// GetOneRandomWorld() (World, error)
	GetOneRandomWorld(*World) error
	UpdateWorlds(Worlds) error
	GetFortunes() (templates.Fortunes, error)
	Close()
}

func RandomWorldNum() int {
	return rand.Intn(worldsCount) + 1
}

// InitDB with appropriate driver
func InitDB(dbDriver, dbConnectionString string, maxConnectionCount int) (DB, error) {
	switch dbDriver {
	case "pgx":
		return NewPgxDB(dbConnectionString)
	case "mongo":
		return NewMongoDB(dbConnectionString, maxConnectionCount)
	case "none":
		return nil, nil
	}

	return nil, errors.New("can not recognize DB driver type")
}
