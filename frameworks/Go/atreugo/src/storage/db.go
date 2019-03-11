package storage

import (
	"errors"
	"fmt"
	"runtime"

	"atreugo/src/templates"
)

const (
	selectQueryStrPostgre  = "SELECT id, randomNumber FROM World WHERE id = $1"
	updateQueryStrPostgre  = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneQueryStrPostgre = "SELECT id, message FROM Fortune"
)

const (
	worldsCount = 10000
)

// DB is interface for
type DB interface {
	// GetOneRandomWorld() (World, error)
	GetOneRandomWorld(*World) error
	UpdateWorlds([]World) error
	GetFortunes() ([]templates.Fortune, error)
	GetFortunesPool() ([]templates.Fortune, error)
	Close()
}

// InitDB with appropriate driver
func InitDB(dbDriver, dbConnectionString string) (DB, error) {
	var err error
	var db DB

	if dbDriver == "pgx" {
		db, err = NewPgxDB(
			dbConnectionString,
			runtime.NumCPU())
		if err != nil {
			return nil, fmt.Errorf("Error opening postgresql database with pgx driver: %s", err)
		}
	} else if dbDriver == "none" {
		db = nil
	} else {
		return nil, errors.New("Can't recognize DB driver type")
	}

	return db, nil
}
