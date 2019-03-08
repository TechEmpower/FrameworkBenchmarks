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
func (mysql MySQL) UpdateWorlds(selectedWorlds []World, queries int) error {
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

// func dbInterpolateHandler(w http.ResponseWriter, r *http.Request) {
// 	var world World
// 	err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world.Id, &world.RandomNumber)
// 	if err != nil {
// 		log.Fatalf("Error scanning world row: %s", err.Error())
// 	}

// 	w.Header().Set("Server", "Go")
// 	w.Header().Set("Content-Type", "application/json")
// 	json.NewEncoder(w).Encode(&world)
// }

// func queriesInterpolateHandler(w http.ResponseWriter, r *http.Request) {
// 	n := getQueriesParam(r)

// 	world := make([]World, n)
// 	for i := 0; i < n; i++ {
// 		err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber)
// 		if err != nil {
// 			log.Fatalf("Error scanning world row: %v", err)
// 		}
// 	}

// 	w.Header().Set("Server", "Go")
// 	w.Header().Set("Content-Type", "application/json")
// 	json.NewEncoder(w).Encode(world)
// }

// func fortuneInterpolateHandler(w http.ResponseWriter, r *http.Request) {
// 	rows, err := db.Query(fortuneSelect)
// 	if err != nil {
// 		log.Fatalf("Error preparing statement: %v", err)
// 	}

// 	fortunes := fetchFortunes(rows)
// 	fortunes = append(fortunes, &Fortune{Message: "Additional fortune added at request time."})

// 	sort.Sort(ByMessage{fortunes})
// 	w.Header().Set("Server", "Go")
// 	w.Header().Set("Content-Type", "text/html; charset=utf-8")
// 	if err := tmpl.Execute(w, fortunes); err != nil {
// 		http.Error(w, err.Error(), http.StatusInternalServerError)
// 	}
// }

// func updateInterpolateHandler(w http.ResponseWriter, r *http.Request) {
// 	n := getQueriesParam(r)

// 	world := make([]World, n)
// 	for i := 0; i < n; i++ {
// 		if err := db.QueryRow(worldSelect, rand.Intn(worldRowCount)+1).Scan(&world[i].Id, &world[i].RandomNumber); err != nil {
// 			log.Fatalf("Error scanning world row: %v", err)
// 		}
// 		world[i].RandomNumber = uint16(rand.Intn(worldRowCount) + 1)
// 		if _, err := db.Exec(worldUpdate, world[i].RandomNumber, world[i].Id); err != nil {
// 			log.Fatalf("Error updating world row: %v", err)
// 		}
// 	}

// 	w.Header().Set("Server", "Go")
// 	w.Header().Set("Content-Type", "application/json")
// 	encoder := json.NewEncoder(w)
// 	encoder.Encode(world)
// }
