package storage

import (
	"context"
	"sort"

	"atreugo/src/templates"

	pgx "github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

// PGX struct
type PGX struct {
	db *pgxpool.Pool
}

// NewPgxDB creates new connection to postgres db with pgx driver
func NewPgxDB(dbConnectionString string) (DB, error) {
	psql := new(PGX)
	if err := psql.Connect(dbConnectionString); err != nil {
		return nil, err
	}

	return psql, nil
}

// Connect create connection and ping db
func (psql *PGX) Connect(dbConnectionString string) error {
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
func (psql *PGX) GetOneRandomWorld(w *World) error {
	id := RandomWorldNum()

	return psql.db.QueryRow(context.Background(), worldSelectSQL, id).Scan(&w.ID, &w.RandomNumber)
}

// UpdateWorlds updates some number of worlds entries, passed as arg
func (psql *PGX) UpdateWorlds(worlds Worlds) error {
	// against deadlocks
	sort.Slice(worlds, func(i, j int) bool {
		return worlds[i].ID < worlds[j].ID
	})

	batch := pgx.Batch{}

	for _, w := range worlds {
		batch.Queue(worldUpdateSQL, w.RandomNumber, w.ID)
	}

	return psql.db.SendBatch(context.Background(), &batch).Close()
}

// GetFortunes selects all fortunes from table
func (psql *PGX) GetFortunes() (templates.Fortunes, error) {
	rows, err := psql.db.Query(context.Background(), fortuneSelectSQL)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	fortunes := templates.AcquireFortunes()
	fortune := templates.AcquireFortune()

	for rows.Next() {
		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			return nil, err
		}

		fortunes = append(fortunes, *fortune)
	}

	templates.ReleaseFortune(fortune)

	return fortunes, nil
}
