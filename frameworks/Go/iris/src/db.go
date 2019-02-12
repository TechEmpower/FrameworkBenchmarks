package main

import (
	"database/sql"
	"fmt"
	"log"
	"math/rand"
	"sort"
)

var (
	selectStmt  *sql.Stmt
	updateStmt  *sql.Stmt
	fortuneStmt *sql.Stmt
)

func getOneRandomWorld() World {
	var w World
	var queryID int
	queryID = rand.Intn(10000) + 1
	if err := selectStmt.QueryRow(queryID).Scan(&w.ID, &w.RandomNumber); err != nil {
		log.Printf("Error scanning world row with ID %d: %s\n", queryID, err)
	}
	return w
}

func updateRandomWorlds(db *sql.DB, queries int) []World {
	selectedWorlds := make([]World, queries)
	for i := 0; i < queries; i++ {
		selectedWorlds[i] = getOneRandomWorld()
	}

	if len(selectedWorlds) > 0 {
		// against deadlocks
		sort.Slice(selectedWorlds, func(i, j int) bool {
			return selectedWorlds[i].ID < selectedWorlds[j].ID
		})

		tx, err := db.Begin()
		if err != nil {
			log.Fatal(err)
		}

		for _, selectedWorld := range selectedWorlds {
			selectedWorld.RandomNumber = rand.Intn(10000) + 1
			if _, err := tx.Stmt(updateStmt).Exec(selectedWorld.RandomNumber, selectedWorld.ID); err != nil {
				log.Printf("Can't update row ID %d with number %d: %s", selectedWorld.ID, selectedWorld.RandomNumber, err)
				tx.Rollback()
			}
		}

		if err := tx.Commit(); err != nil {
			log.Fatal(err)
		}
	}

	return selectedWorlds
}

func mustPrepare(db *sql.DB, query string) (*sql.Stmt, error) {
	stmt, err := db.Prepare(query)
	if err != nil {
		log.Printf("Error when preparing statement %q: %s\n", query, err)
		return nil, err
	}
	return stmt, nil
}

func initDatabase(dbHost string, dbUser string, dbPass string, dbName string, maxConnectionsInPool int) (*sql.DB, error) {
	var err error
	var db *sql.DB
	db, err = sql.Open("postgres",
		fmt.Sprintf("host=%s user=%s password=%s dbname=%s sslmode=disable",
			dbHost,
			dbUser,
			dbPass,
			dbName,
		),
	)
	if err != nil {
		return nil, err
	}

	if err = db.Ping(); err != nil {
		return nil, err
	}

	maxConnectionsInPool = maxConnectionsInPool * 4
	db.SetMaxOpenConns(maxConnectionsInPool)
	db.SetMaxIdleConns(maxConnectionsInPool)

	if selectStmt, err = mustPrepare(db, "SELECT id, randomNumber FROM World WHERE id = $1"); err != nil {
		return nil, err
	}
	if fortuneStmt, err = mustPrepare(db, "SELECT id, message FROM Fortune"); err != nil {
		return nil, err
	}
	if updateStmt, err = mustPrepare(db, "UPDATE World SET randomNumber = $1 WHERE id = $2"); err != nil {
		return nil, err
	}

	return db, nil
}
