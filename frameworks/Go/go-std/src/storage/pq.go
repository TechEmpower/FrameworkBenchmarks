package storage

// import (
// 	"database/sql"
// 	"fmt"
// 	"log"
// 	"math/rand"
// 	"sort"

// 	"go-std/src/templates"

// 	_ "github.com/lib/pq" // postgresql import
// )

// // PQ struct
// type PQ struct {
// 	db *sql.DB
// 	// prepare statements
// 	selectStmt  *sql.Stmt
// 	updateStmt  *sql.Stmt
// 	fortuneStmt *sql.Stmt
// }

// // Connect create connection and ping db
// func (psql *PQ) Connect(dbConnectionString string, maxConnectionsInPool int) error {
// 	var err error
// 	psql.db, err = sql.Open("postgres", dbConnectionString)
// 	if err != nil {
// 		return err
// 	}

// 	err = psql.db.Ping()
// 	if err != nil {
// 		return err
// 	}

// 	psql.db.SetMaxOpenConns(maxConnectionsInPool)
// 	psql.db.SetMaxIdleConns(maxConnectionsInPool)

// 	if psql.selectStmt, err = psql.mustPrepare(selectQueryStrPostgre); err != nil {
// 		return err
// 	}
// 	if psql.fortuneStmt, err = psql.mustPrepare(fortuneQueryStrPostgre); err != nil {
// 		return err
// 	}
// 	if psql.updateStmt, err = psql.mustPrepare(updateQueryStrPostgre); err != nil {
// 		return err
// 	}

// 	return nil
// }

// // Close connect to db
// func (psql *PQ) Close() {
// 	psql.db.Close()
// }

// // GetOneRandomWorld return one random World struct
// func (psql PQ) GetOneRandomWorld() (World, error) {
// 	var w World
// 	var err error
// 	queryID := rand.Intn(worldsCount) + 1
// 	if err = psql.selectStmt.QueryRow(queryID).Scan(&w.ID, &w.RandomNumber); err != nil {
// 		err = fmt.Errorf("error scanning world row with ID %d: %s", queryID, err)
// 	}
// 	return w, err
// }

// // UpdateRandomWorlds updates some number of worlds entries, passed as arg
// func (psql PQ) UpdateRandomWorlds(queries int) ([]World, error) {
// 	selectedWorlds := make([]World, queries)
// 	for i := 0; i < queries; i++ {
// 		selectedWorlds[i], _ = psql.GetOneRandomWorld()
// 	}

// 	if len(selectedWorlds) > 0 {
// 		// against deadlocks
// 		sort.Slice(selectedWorlds, func(i, j int) bool {
// 			return selectedWorlds[i].ID < selectedWorlds[j].ID
// 		})

// 		tx, err := psql.db.Begin()
// 		if err != nil {
// 			return selectedWorlds, err
// 		}

// 		for _, selectedWorld := range selectedWorlds {
// 			selectedWorld.RandomNumber = rand.Intn(worldsCount) + 1
// 			if _, err := tx.Stmt(psql.updateStmt).Exec(selectedWorld.RandomNumber, selectedWorld.ID); err != nil {
// 				log.Printf("Can't update row ID %d with number %d: %s", selectedWorld.ID, selectedWorld.RandomNumber, err)
// 				tx.Rollback()
// 			}
// 		}

// 		if err := tx.Commit(); err != nil {
// 			tx.Rollback()
// 			return selectedWorlds, err
// 		}
// 	}

// 	return selectedWorlds, nil
// }

// // GetFortunes selects all fortunes from table
// func (psql PQ) GetFortunes() ([]templates.Fortune, error) {
// 	rows, err := psql.fortuneStmt.Query()
// 	defer rows.Close()
// 	if err != nil {
// 		return nil, fmt.Errorf("can't query fortunes: %s", err)
// 	}

// 	fortunes := make([]templates.Fortune, 0, 16)
// 	var fortune templates.Fortune
// 	for rows.Next() {
// 		if err = rows.Scan(&fortune.ID, &fortune.Message); err != nil {
// 			log.Printf("Can't scan fortune: %s\n", err)
// 		}
// 		fortunes = append(fortunes, fortune)
// 	}

// 	return fortunes, nil
// }

// func (psql PQ) mustPrepare(query string) (*sql.Stmt, error) {
// 	stmt, err := psql.db.Prepare(query)
// 	if err != nil {
// 		log.Printf("Error when preparing statement %q: %s\n", query, err)
// 		return nil, err
// 	}
// 	return stmt, nil
// }

// // NewPqDB creates new connection to postgres db with pq driver
// func NewPqDB(dbConnectionString string, maxConnectionsInPool int) (DB, error) {
// 	var psql PQ
// 	if err := psql.Connect(dbConnectionString, maxConnectionsInPool); err != nil {
// 		return nil, err
// 	}
// 	return &psql, nil
// }
