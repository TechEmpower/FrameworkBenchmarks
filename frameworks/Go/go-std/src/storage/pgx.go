package storage

import (
	"fmt"
	"log"
	"math/rand"
	"sort"

	"go-std/src/templates"

	"github.com/jackc/pgx"
)

// PGX struct
type PGX struct {
	db *pgx.ConnPool
	// prepare statements
	selectStmt  *pgx.PreparedStatement
	updateStmt  *pgx.PreparedStatement
	fortuneStmt *pgx.PreparedStatement
}

// Connect create connection and ping db
func (psql *PGX) Connect(dbConnectionString string, maxConnectionsInPool int) error {
	var parsedConfig pgx.ConnConfig
	var err error
	parsedConfig, err = pgx.ParseConnectionString(dbConnectionString)
	if err != nil {
		return err
	}

	var config pgx.ConnPoolConfig
	config.User = parsedConfig.User
	config.Host = parsedConfig.Host
	config.Password = parsedConfig.Password
	config.Database = parsedConfig.Database

	config.MaxConnections = maxConnectionsInPool

	config.AfterConnect = func(conn *pgx.Conn) error {
		var err error
		if psql.selectStmt, err = mustPrepare(conn, "selectStmt", selectQueryStrPostgre); err != nil {
			return err
		}
		if psql.updateStmt, err = mustPrepare(conn, "updateStmt", updateQueryStrPostgre); err != nil {
			return err
		}
		if psql.fortuneStmt, err = mustPrepare(conn, "fortuneStmt", fortuneQueryStrPostgre); err != nil {
			return err
		}
		return nil
	}

	psql.db, err = pgx.NewConnPool(config)
	if err != nil {
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
	if err = psql.db.QueryRow("selectStmt", queryID).Scan(&w.ID, &w.RandomNumber); err != nil {
		err = fmt.Errorf("error scanning world row with ID %d: %s", queryID, err)
	}
	return err
}

// UpdateWorlds updates some number of worlds entries, passed as arg
func (psql PGX) UpdateWorlds(selectedWorlds []World) error {
	// against deadlocks
	sort.Slice(selectedWorlds, func(i, j int) bool {
		return selectedWorlds[i].ID < selectedWorlds[j].ID
	})

	tx, err := psql.db.Begin()
	if err != nil {
		return err
	}

	for _, selectedWorld := range selectedWorlds {
		selectedWorld.RandomNumber = rand.Intn(worldsCount) + 1
		if _, err := tx.Exec("updateStmt", selectedWorld.RandomNumber, selectedWorld.ID); err != nil {
			log.Printf("Can't update row ID %d with number %d: %s", selectedWorld.ID, selectedWorld.RandomNumber, err)
			tx.Rollback()
		}
	}

	// for i := 0; i < queries; i++ {
	// 	selectedWorlds[i].RandomNumber = rand.Intn(worldsCount) + 1
	// 	if _, err := tx.Exec("updateStmt", selectedWorlds[i].RandomNumber, selectedWorlds[i].ID); err != nil {
	// 		log.Printf("Can't update row ID %d with number %d: %s", selectedWorlds[i].ID, selectedWorlds[i].RandomNumber, err)
	// 		tx.Rollback()
	// 	}
	// }

	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return err
	}

	return nil
}

// GetFortunes selects all fortunes from table
func (psql PGX) GetFortunes() ([]templates.Fortune, error) {
	rows, err := psql.db.Query("fortuneStmt")
	defer rows.Close()
	if err != nil {
		return nil, fmt.Errorf("can't query fortunes: %s", err)
	}

	fortunes := make([]templates.Fortune, 0, 16)
	var fortune templates.Fortune
	for rows.Next() {
		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Printf("Can't scan fortune: %s\n", err)
		}
		fortunes = append(fortunes, fortune)
	}

	return fortunes, nil
}

// GetFortunesPool selects all fortunes from table
func (psql PGX) GetFortunesPool() ([]templates.Fortune, error) {
	rows, err := psql.db.Query("fortuneStmt")
	defer rows.Close()
	if err != nil {
		return nil, fmt.Errorf("can't query fortunes: %s", err)
	}

	fortunes := templates.FortunesPool.Get().([]templates.Fortune)
	var fortune templates.Fortune
	for rows.Next() {
		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
			log.Printf("Can't scan fortune: %s\n", err)
		}
		fortunes = append(fortunes, fortune)
	}

	return fortunes, nil
}

func mustPrepare(db *pgx.Conn, name, query string) (*pgx.PreparedStatement, error) {
	stmt, err := db.Prepare(name, query)
	if err != nil {
		log.Printf("Error when preparing statement %q: %s\n", query, err)
		return nil, err
	}
	return stmt, nil
}

// NewPgxDB creates new connection to postgres db with pgx driver
func NewPgxDB(dbConnectionString string, maxConnectionsInPool int) (DB, error) {
	var psql PGX
	if err := psql.Connect(dbConnectionString, maxConnectionsInPool); err != nil {
		return nil, err
	}
	return &psql, nil
}
