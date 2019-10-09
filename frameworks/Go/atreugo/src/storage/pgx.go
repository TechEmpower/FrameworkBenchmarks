package storage

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"sort"

	"atreugo/src/templates"

	"github.com/jackc/pgx/v4/pgxpool"
)

// PGX struct
type PGX struct {
	db *pgxpool.Pool
}

// Connect create connection and ping db
func (psql *PGX) Connect(dbConnectionString string, maxConnectionsInPool int) error {
	var err error

	if psql.db, err = pgxpool.Connect(context.Background(), dbConnectionString); err != nil {
		return err
	}

	return nil
}

// Close connect to db
func (psql *PGX) Close() {
	psql.db.Close()
}

// GetOneRandomWorld return one random World struct
func (psql PGX) GetOneRandomWorld(w *World) error {
	var err error
	queryID := rand.Intn(worldsCount) + 1
	if err = psql.db.QueryRow(context.Background(), selectQueryStrPostgre, queryID).Scan(&w.ID, &w.RandomNumber); err != nil {
		err = fmt.Errorf("error scanning world row with ID %d: %s", queryID, err)
	}
	return err
}

// UpdateWorlds updates some number of worlds entries, passed as arg
func (psql PGX) UpdateWorlds(selectedWorlds Worlds) error {
	// against deadlocks
	sort.Slice(selectedWorlds, func(i, j int) bool {
		return selectedWorlds[i].ID < selectedWorlds[j].ID
	})

	ctx := context.Background()

	tx, err := psql.db.Begin(ctx)
	if err != nil {
		return err
	}

	for _, selectedWorld := range selectedWorlds {
		selectedWorld.RandomNumber = rand.Intn(worldsCount) + 1

		if _, err := tx.Exec(ctx, updateQueryStrPostgre, selectedWorld.RandomNumber, selectedWorld.ID); err != nil {
			log.Printf("Can't update row ID %d with number %d: %s", selectedWorld.ID, selectedWorld.RandomNumber, err)
			tx.Rollback(ctx)
		}
	}

	if err := tx.Commit(ctx); err != nil {
		tx.Rollback(ctx)
		return err
	}

	return nil
}

// GetFortunes selects all fortunes from table
func (psql PGX) GetFortunes() (templates.Fortunes, error) {
	rows, err := psql.db.Query(context.Background(), fortuneQueryStrPostgre)
	defer rows.Close()
	if err != nil {
		return nil, fmt.Errorf("can't query fortunes: %s", err)
	}

	fortunes := templates.AcquireFortunes()
	fortune := templates.AcquireFortune()

	for rows.Next() {
		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Printf("Can't scan fortune: %s\n", err)
		}
		fortunes = append(fortunes, *fortune)
	}

	templates.ReleaseFortune(fortune)

	return fortunes, nil
}

// NewPgxDB creates new connection to postgres db with pgx driver
func NewPgxDB(dbConnectionString string, maxConnectionsInPool int) (DB, error) {
	psql := new(PGX)
	if err := psql.Connect(dbConnectionString, maxConnectionsInPool); err != nil {
		return nil, err
	}

	return psql, nil
}
