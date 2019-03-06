package storage

import (
	"errors"
	"fmt"
	"iris/src/templates"
	"runtime"
)

const (
	selectQueryStr  = "SELECT id, randomNumber FROM World WHERE id = $1"
	updateQueryStr  = "UPDATE World SET randomNumber = $1 WHERE id = $2"
	fortuneQueryStr = "SELECT id, message FROM Fortune"
)

var (
	worldsCount = 10000
)

// DB is interface for
type DB interface {
	GetOneRandomWorld() (World, error)
	UpdateRandomWorlds(queries int) ([]World, error)
	GetFortunes() ([]templates.Fortune, error)
	Close()
}

// InitDB with appropriate driver
func InitDB(dbDriver, dbConnectionString string) (DB, error) {
	var err error
	var db DB
	if dbDriver == "pq" {
		db, err = NewPqDB(
			dbConnectionString,
			runtime.NumCPU())
		if err != nil {
			return nil, fmt.Errorf("Error opening pq database: %s", err)
		}
	} else if dbDriver == "pgx" {
		db, err = NewPgxDB(
			dbConnectionString,
			runtime.NumCPU())
		if err != nil {
			return nil, fmt.Errorf("Error opening pgx database: %s", err)
		}
	} else if dbDriver == "none" {
		db = nil
	} else {
		return nil, errors.New("Can't recognize DB driver type")
	}

	return db, nil
}
