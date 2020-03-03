package storage

import (
	"database/sql"
	"fmt"
	"log"
	"math/rand"
	"sort"

	"go-std/src/templates"

	_ "github.com/go-sql-driver/mysql" // postgresql import
)

// MySQL struct
type MySQL struct {
	db *sql.DB
	// prepare statements
	selectStmt  *sql.Stmt
	updateStmt  *sql.Stmt
	fortuneStmt *sql.Stmt
}

// Connect create connection and ping db
func (mysql *MySQL) Connect(dbConnectionString string, maxConnectionsInPool int) error {
	var err error
	mysql.db, err = sql.Open("mysql", dbConnectionString)
	if err != nil {
		return err
	}

	err = mysql.db.Ping()
	if err != nil {
		return err
	}

	mysql.db.SetMaxOpenConns(maxConnectionsInPool)
	mysql.db.SetMaxIdleConns(maxConnectionsInPool)

	if mysql.selectStmt, err = mysql.mustPrepare(selectQueryStrMySQL); err != nil {
		return err
	}
	if mysql.fortuneStmt, err = mysql.mustPrepare(fortuneQueryStrMySQL); err != nil {
		return err
	}
	if mysql.updateStmt, err = mysql.mustPrepare(updateQueryStrMySQL); err != nil {
		return err
	}

	return nil
}

// Close connect to db
func (mysql *MySQL) Close() {
	mysql.db.Close()
}

// GetOneRandomWorld return one random World struct
func (mysql MySQL) GetOneRandomWorld(w *World) error {
	var err error
	queryID := rand.Intn(worldsCount) + 1
	if err = mysql.selectStmt.QueryRow(queryID).Scan(&w.ID, &w.RandomNumber); err != nil {
		err = fmt.Errorf("error scanning world row with ID %d: %s", queryID, err)
	}
	return err
}

// UpdateWorlds updates some number of worlds entries, passed as arg
func (mysql MySQL) UpdateWorlds(selectedWorlds []World) error {
	// against deadlocks
	sort.Slice(selectedWorlds, func(i, j int) bool {
		return selectedWorlds[i].ID < selectedWorlds[j].ID
	})

	tx, err := mysql.db.Begin()
	if err != nil {
		return err
	}

	for _, selectedWorld := range selectedWorlds {
		selectedWorld.RandomNumber = rand.Intn(worldsCount) + 1
		if _, err := tx.Stmt(mysql.updateStmt).Exec(selectedWorld.RandomNumber, selectedWorld.ID); err != nil {
			log.Printf("Can't update row ID %d with number %d: %s", selectedWorld.ID, selectedWorld.RandomNumber, err)
			tx.Rollback()
		}
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return err
	}

	return nil
}

// GetFortunes selects all fortunes from table
func (mysql MySQL) GetFortunes() ([]templates.Fortune, error) {
	rows, err := mysql.fortuneStmt.Query()
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
func (mysql MySQL) GetFortunesPool() ([]templates.Fortune, error) {
	rows, err := mysql.fortuneStmt.Query()
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

func (mysql MySQL) mustPrepare(query string) (*sql.Stmt, error) {
	stmt, err := mysql.db.Prepare(query)
	if err != nil {
		log.Printf("Error when preparing statement %q: %s\n", query, err)
		return nil, err
	}
	return stmt, nil
}

// NewMySQLDB creates new connection to postgres db with MySQL driver
func NewMySQLDB(dbConnectionString string, maxConnectionsInPool int) (DB, error) {
	var mysql MySQL
	if err := mysql.Connect(dbConnectionString, maxConnectionsInPool); err != nil {
		return nil, err
	}
	return &mysql, nil
}
